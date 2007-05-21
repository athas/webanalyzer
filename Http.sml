(* http-klient funktionalitet i ML. ander@diku.dk 
   Benytter Socket og RegEx modulerne. *)

structure Http :> Http =
struct

(* Failure kan indeholde tre forskellige slags
   fejlmeddelelser. HTTP har en status-kode 
   defineret i http-protokollen og dernæst en
   forklarende tekstbesked. Socket bruges hvis 
   et problem er opstået med netværkskommunikationen. 
   Endelig vil alle andre fejl-meldinger være 
   dækket af General. *)

datatype Failure = HTTP of int * string
                 | Socket of string
                 | General of string;

exception Error of Failure;

(* failString: Failure -> string

   Laver fejlkode udfra Failure. *)
fun failString (General s) = s
  | failString (Socket s) = s
  | failString (HTTP (n, s)) 
     = s ^ "[" ^ (Int.toString n) ^ "]";

open Util;

(* Import af enkelte navne fra Substring til globalt navnerum *)
val size = Substring.size;
val str = Substring.string;

(* lowercase: string -> string

   Laver ny streng med udelukkende små bogstaver *)
fun lowercase str = implode(map Char.toLower (explode str));

(* get: char vector * int -> string

   Henter streng ud af vector *)
fun get args = str (Vector.sub args);

(* default: 'a * 'a option -> 'a

   Returnerer value hvis muligt ellers std. *)
fun default std NONE = std
  | default std (SOME value) = value;

type address = string * int option;

(* parseAddress: string -> string * int option

   server består af selve server-adresse, som enten
   navn eller ip-adresse, eventuelt efterfulgt af kolon 
   og portnummer. Der returneres: name/ip : string
                                  port    : int option
   MalformedAddress kastes hvis server ikke matcher det
   forventede format.  *)
exception MalformedAddress of string;
fun parseAddress addr =
let
    val regstr = "([^:]+)(:([0-9]+))?";
    val regexp = Regex.regcomp regstr [Regex.Extended, Regex.Icase]
    val match = Regex.regexec regexp [] addr
    val (s, port') = 
            case match of NONE   => raise MalformedAddress addr
                        | SOME v => (Vector.sub(v,1), Vector.sub(v,3))
    val port'' = if size port' = 0 then NONE 
		 else Int.fromString (str port')
    val port = if (isSome port'') andalso (valOf port'') = 80 then NONE
               else port''
in
    if (size s) = 0 then raise MalformedAddress addr
    else (str s, port)
end;

(* parseURI: bool -> string -> string option * address option 
                             * string option

   parseURI vil trække enkeltfelter ud af en uri.
   MalformedURI hvis det går galt. isHTML angiver at server-angivelse
   skal startes med //. 
   En URI har følgende struktur:
        [protocol://][server[:port]][/path] eller
        [protocol:][//server[:port]][/path] hvis isHTML
   Alle elementer kan udelades og dette markeres ved NONE.
   Der returneres en trippel (protocol : string option,
                              server   : address option)
                              path     : string option)
   og kastes undtagelsen MalformedURI hvis uri-strengen
   ikke giver mening. *)
exception MalformedURI of string;
fun parseURI isHTML uri =
let
    val regstr = if isHTML then "(([a-z]+)()?:)?(//([^/]+))?(.+)?"
                 else "((([a-z]+):)?//)?(([^/]+))?(.+)?";
    val regexp = Regex.regcomp regstr [Regex.Extended, Regex.Icase]
    val match = Regex.regexec regexp [] uri
    val (prot', addr', path') = 
            case match of NONE   => raise MalformedURI uri
                        | SOME v => (Vector.sub(v,3), Vector.sub(v,5),
                                     Vector.sub(v,6))
    val prot = if size prot' = 0 then NONE else SOME(lowercase (str prot'))
    val addr = if size addr' = 0 then NONE else SOME(lowercase (str addr'))
    val path = if size path' = 0 then NONE else SOME(str path')
    val address = case addr of NONE => NONE
                           | SOME s => SOME(parseAddress s)
in
    (prot, address, path)
end;

type URI = string     (* protokol   *)
         * string     (* værts-navn *)
         * int option (* værts-port *)
         * string;    (* sti *)

(* protocolFromURI: URI -> string
   serverFromURI:   URI -> string
   pathFromURI:     URI -> string
   stringFromURI:   URI -> string

   stringFromURI bygger en komplet uri som streng. 
   De andre returnerer blot enkelte elementer. *)
fun protocolFromURI (protocol, _, _, _) = protocol;
fun serverFromURI (_, server, port, _) = server ^ 
    (if not (Option.isSome port) then "" 
     else ":" ^ Int.toString (Option.valOf port));
fun pathFromURI (_, _, _, path) = path;
fun stringFromURI uri = 
    (protocolFromURI uri) ^ "://" ^ (serverFromURI uri) ^ (pathFromURI uri);
    
(* readAll: ('a, active stream) sock -> string

   Indlæs data indtil socket lukkes.
   Det er ikke umiddelbart at se hvornår en Socket er
   lukket på server-siden. Derfor stopper denne funktion
   med at indlæse data, når der ikke har været noget
   i tyve sekunder eller der modtages en undtagelse.
   For at øge hastigheden afsluttes også når strengen </html>
   mødes. *)
val timeout = 20.0;
fun readAll socket =
let fun now () = Time.toReal (Time.now())
    fun moreHTML text = 
    let val regstr = "</html>";
        val regexp = Regex.regcomp regstr [Regex.Extended, Regex.Icase]
        val match = Regex.regexec regexp [] text
    in  not (isSome match) end;
    fun read(acc, time) = if not (moreHTML acc) then acc else
    let
        val vector = Socket.recvVec'(socket, 1, {peek=true, oob=false}) 
        (* Data må IKKE indlæses hvis der ikke er noget parat.
           Ved at pakke indlæsningen ind i en anonym funktion,
           kan dette undgås.                                   *) 
        val data = fn () => Socket.recvVec(socket, 1000) 
        val length = Word8Vector.length vector
        val msecs = now() - time
    in  
        if length > 0 then 
            let val str = (Byte.bytesToString (data()))
            in  read(acc ^ str, now())        
        end handle _ => acc
        else if msecs <= timeout then read(acc, time) 
             else acc
    end
in  
    read("", now())
end;

(* readString: ('a, active stream) sock -> int -> string

   Læs n tegn fra socket. *) 
fun readString socket n =
let val vector = Socket.recvVec(socket, n) 
                 handle Fail str => raise Error(Socket str)
    val str = Byte.bytesToString vector 
    val len = String.size str
in  if len >= n then str else str ^ readString socket (n - len)
end;

(* writeSocket: ('a, active stream) sock -> string -> int
   
   Skriver streng til socket. *)
fun writeSocket socket str =
let 
    val reqVec = {buf=Byte.stringToBytes str, ofs=0, size=NONE}
in  
    Socket.sendVec(socket, reqVec)
    handle Fail s => raise Error(Socket ("Error writing data: " ^ s))
end;

(* resolveAddress: address -> pf_inet sock_addr

   Oversætter adresse til internt socket-format. *)
fun resolveAddress(host, port) =
(Socket.inetAddr (gethostbyname host) (default 80 port)
)handle Fail str => raise Error (General str);

(* readResponseHeader: ('a, active stream) sock -> 
                       int * (string * string) list

   Indlæser HTTP 1.0+ svar og returnerer status-kode samt 
   liste af tupler (nøgle, værdi) der beskriver svaret. 
   Hvis svaret ikke er gyldigt rejses en Header undtagelse med hele 
   det ugyldige svar. *)

exception Header of string;
fun readResponseHeader socket =
let 
    val line1 = readLine socket
    val regstr = "HTTP/([0-9]+\\.[0-9]+)[[:space:]]+([0-9]+)[[:space:]]+(.*)"
    val regexp = Regex.regcomp regstr [Regex.Extended, Regex.Icase]
    val match = Regex.regexec regexp [] line1
in  
    case match of NONE => raise Header line1
              | SOME v => 
    let val status = valOf(Int.fromString (get (v, 2)))
                     handle Option => raise Fail line1
        val regstr = "([^:]+):[[:space:]]*([^;[:space:]]+)"
        val regexp = Regex.regcomp regstr [Regex.Extended, Regex.Icase]
        (* gemmer liste af tupler (nøgle, værdi) indtil en 
           helt tom linie dukker op *)
        fun getHeader () =
        let val line = readLine socket
        in  if line = "\r\n" then [] else 
                (case (Regex.regexec regexp [] line) of 
                     NONE => []
                   | SOME v => [(lowercase (str (Vector.sub(v,1))),
                                            str (Vector.sub(v, 2)))]
                ) @ getHeader ()
        end
        val header = getHeader()
                     handle Fail str => raise Error(Socket str)
    in  (status, header)
    end
end;

(* get: (string * string) list * string -> string option

   Leder gennem liste af tupler efter værdien 
   der matcher nøglen. *)
fun get([], key) = NONE
  | get((k,v)::ts, key) = 
    if k = key then SOME v  
    else get(ts, key);
	
(* getResponse: ('a, active stream) sock -> string * string

   Indlæser svaret fra server på en tidligere afsendt forespørgsel.
   Der indlæses indholds-type (eng: content-type) og selve indholdet,
   der ikke nødvendigvis er html. Funktionen udøver sort magi, hvis 
   ikke man er intim med http-protokollen. Læs eventuelt afsnit fem 
   og seks af rfc 2616. *)

fun getResponse socket =
(let val (status, header) = readResponseHeader socket
     val _ = if status < 200 orelse status >= 400 
             then raise Error (HTTP (status, readAll socket)) else () 
     val cLength' = get(header, "content-length")
     val cLength = case cLength' of NONE => NONE
                                | SOME s => Int.fromString s
     val cType   = default "text/html" (get (header, "content-type"))
 in  (cType, case cLength of 
               NONE => readAll socket 
           | SOME l => readString socket l)
 end
) handle Header str => ("text/html", str ^ (readAll socket));

(* headResponse: ('a, active stream) sock -> string option

   Indlæser kanonisk uri fra server udfra tidligere afsendt 
   head forespørgsel. Hvis serveren ikke angiver kanonisk uri
   bliver NONE returneret. Ved egentlig fejl kastes undtagelse. 
   Hvis content-type er angivet som andet end text/html kastes
   en Content undtagelse. *)
exception Content of string;
fun headResponse socket =
(let val (status, header) = readResponseHeader socket
     val _ = if status < 200 orelse status >= 400 
             then raise Error (HTTP (status, "")) else () 
     val l' = get(header, "location")
     val l  = if isSome l' then l' 
              else get(header, "content-location")
     val cType = default "text/html" (get (header, "content-type"))
 in l end) handle Header str => NONE;

(* requestHTTPbyServer : (('a, active stream) sock -> 'a * string) -> 
                         pf_inet sock_addr -> 'a

   Laver http-forespørgsel som angivet ved request-strengen og 
   håndterer svaret ved hjælp af receiver. *)
fun requestHTTPbyServer (receiver, request) addr =
    let 
        val server = Socket.getinetaddr addr
        val socket = Socket.inetStream()
    in (Socket.connect(socket, addr)
        handle Fail str => raise Error (Socket ("Error connecting to " ^ 
                                                server ^ ": " ^ str));
        writeSocket socket request;
        receiver socket before 
        Socket.close socket)
       handle e => (Socket.close socket; raise e)
    end;

(* requestHTTP: (('a, active stream) sock -> 'a * string) ->
                address -> 'a 

   Laver http-forespørgsel givet server-adresse og request.
   Svaret bestemmes af receiver. *)

fun requestHTTP (receiver, request) host = 
    requestHTTPbyServer (receiver, request) (resolveAddress host);

(* HTTP-forespørgsler udenfor diku skal gennem en proxy-server.
   Sidder man ikke på diku er proxy-serveren dog ikke tilgængelig.

   method vil beskrive hvorledes en forespørgsel skal foretages.
   Direct vil lave en direkte forbindelse, mens
   Proxy vil benytte proxy-server. 
   Automatic vil prøve både proxy-server og direkte forbindelse. *)
val proxy = "proxy:8080";
datatype Config = Automatic of Socket.pf_inet Socket.sock_addr
                | Proxy of Socket.pf_inet Socket.sock_addr
                | Direct;
val method = ref (Automatic (resolveAddress (parseAddress proxy))) handle _ => ref Direct; 

(* buildReq: string * string * string -> string 

   Bygger http-request linie udfra action, path og host. *)
fun buildReq (action, path, host) = action ^ " " ^ path ^ " HTTP/1.1\r\n" ^
                                    "Host: " ^ host ^ "\r\n\r\n";

(*  val requestURI': (('a, active stream) sock -> 'a * string) -> 
                     Config -> string -> URI -> 'a 

    requestURI' vil lave http-forespørgsel udfra method, action (head/get) 
    og uri, mens svaret bestemmes af receiver-funktionen. *)
fun requestURI' (receiver, action) method uri = 
case method of Direct =>
    let val (protocol, host, port, path) = uri 
        val server = (host, port)
        val request = buildReq(action, path, serverFromURI uri)
        (* val request = buildReq(action, path) *)
    in
        if protocol = "http" then requestHTTP (receiver, request) server
        else raise Error(General("Bad protocol: " ^ protocol))
    end
| Proxy addr => 
    let val request = buildReq(action, stringFromURI uri, serverFromURI uri)  
    in  requestHTTPbyServer (receiver, request) addr end
| _ => raise Error(General "Bad getURI' method");

(* requestURI: (('a, active stream) sock -> 'a * string) -> URI -> 'a 

   requestURI vil forsøge forbindelse gennem proxy eller direkte indtil
   en af disse metoder går godt første gang. Fra da af vil metoden
   der fungerede blive brugt. *)
fun requestURI (receiver, action) uri = 
let val response = case !method of 
        Automatic addr => let fun proxyOff () = method := Direct
                              fun proxyOn  () = method := Proxy addr
                          in (requestURI' (receiver, action) (Proxy addr) uri
                              before proxyOn())
                             handle _ => (requestURI' (receiver, action) Direct uri 
                                          before proxyOff())
                          end
      | m => requestURI' (receiver, action) m uri
in response end;

(* getURI: URI -> string

   getURI vil foretage en http-get forespørgsel via enten proxy eller
   direkte.*) 
fun getURI uri = 
let val (ctype, content) = (requestURI (getResponse, "GET") uri)
                           handle Error e => raise Error e
                                |       _ => raise Error(General "Unknown failure")
in content end;

(* buildURI': URI option * string -> URI

   En ny URI konstrueres udfra streng og eventuelt
   tidligere URI som strengen skal ses relativt til. 
   Opbygning af stier klares af Path-modulet. *)
exception badURI;
fun buildURI' (origin : URI option, str) = 
let open Option
    val (protocol, server, path) = (parseURI (isSome origin) str)
                                    handle _ => raise badURI
in  if isSome origin then 
    let fun joinPath (orig, new) =
        let open Path
        in  if isAbsolute new then new else 
	    mkCanonical (concat (dir orig, mkCanonical new))
        end
        val origin' = valOf origin
        val protocol' = default (#1 origin') protocol
        val server' = default (#2 origin', #3 origin') server
        val path' = if not (isSome path) then "/" 
                    else joinPath(#4 origin', valOf path)
        val name = #1 server'
        val port = if isSome (#2 server') then (#2 server')
                   else #3 origin'
    in  (protocol', name, port, path') end else
    if not (isSome server) then raise badURI else
    let val protocol' = default "http" protocol
        val server' = valOf server
        val name = #1 server'
        val port = #2 server'
        val path' = default "/" path
    in  (protocol', name, port, Path.mkCanonical path') end
end;

(* canonicalURI: URI -> URI

   Der tages kontankt til server uri ligger på
   og en head-forespørgsel foretages for at ændre
   uri, hvis ny location gives. Der kastes HTTP 
   undtagelser hvis kommunikation med server slår fejl. *)
fun canonicalURI orig = 
    let fun getHead uri = requestURI (headResponse, "HEAD") uri
            handle Error (HTTP (int, string)) => NONE
    in case (getHead orig) of NONE => orig
                            | SOME uri => buildURI'(SOME orig, uri)
end;

(* buildURI: URI option * string -> URI

   buildURI' vil opbygge en relativ og absolut URI
   udfra givne oplysninger. Der tages kontakt til webserver
   for så kun kanoniske uri returneres. Tvetydighed mht. 
   partielle eller absolutte uri løses også ved forespørgsel
   hos server. *)
fun buildURI (SOME uri, new) = 
    (canonicalURI (buildURI'(SOME uri, new))
     handle Error _ => buildURI(NONE, new)
          |       _ => raise badURI)
  | buildURI (NONE, new) = 
    (canonicalURI (buildURI'(NONE, new))
     handle _ => raise badURI)

(* occurrences: string -> string list 

   Skanner strengen for alt hvad der kan ligne uri'er, mere
   konkret kombinationer af følgende strenge. Altså kan der
   bliver fundet ugyldige uri'er som ikke er indeni anker-tags.
     href =  uri
     rel  = "uri"
     src  = 'uri' *)
local
 val head = "((rel)|(href)|(src))[[:space:]]*=[[:space:]]*";
 val regstr1 = head ^ "([^'\"[:space:]][^>[:space:]]+)"; 
 val regstr2 = head ^ "\"([^\"]*)\"";    
 val regstr3 = head ^ "'([^']*)'"; 
 val regexp1 = Regex.regcomp regstr1 [Regex.Extended, Regex.Icase];
 val regexp2 = Regex.regcomp regstr2 [Regex.Extended, Regex.Icase];
 val regexp3 = Regex.regcomp regstr3 [Regex.Extended, Regex.Icase];

 fun unique [] = []
   | unique (x::xs) =
 let fun rem(_, []) = []
       | rem(r, s::ss) = if (lowercase s) = r then rem(r, ss)
                         else s :: rem(r, ss)
 in  x :: unique (rem (lowercase x, xs)) end
in
 fun occurrences html =
 let fun get v = Substring.string (Vector.sub (v,5))
     val match1 = Regex.map regexp1 get html
     val match2 = Regex.map regexp2 get html
     val match3 = Regex.map regexp3 get html
     val match = match1 @ match2 @ match3
     open Substring
     fun fix s = string (takel (fn c => c <> #"#") (all s))
     val match' = map fix match
 in  unique match' end
end;

(* local
    val uri = buildURI'(NONE, "http://www.diku.dk/students")
    val refs = occurrences (getURI uri);
    fun inside (x, ys) = List.exists (fn y => x = y) ys
    val ok = inside("http://www.diku.dk/students/", refs)
in  
    val _ = if ok then print "ok\n" else print "argh\n"
end; *)

end;

