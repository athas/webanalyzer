(* Util indeholder netværksfunktionalitet der bruges af
   server.sml og Http modulet. ander@diku.dk *) 
structure Util :> Util =
struct

type 'a sock = ('a, Socket.active Socket.stream) Socket.sock

(* readChar: ('a, active stream) sock -> char

   læs enkelt tegn fra socket *)
fun readChar socket =
let 
    val vector = Socket.recvVec(socket, 1) 
    val str = Byte.bytesToString vector
in  
    hd (explode str)
end;

(* readLine: ('a, active stream) sock -> string
   
   læs indtil #"\n" eller socket lukkes. *)
fun readLine socket =
let 
    fun read () =
    (let val c = readChar socket      
     in  (* debug (implode [c]); *)
         if c = #"\n" then [c]
         else c :: read() 
     end
    ) handle Empty => []
in
    implode (read())
end; 

(* readEmptyLine: ('a, active stream) sock -> string
   
   læs indtil helt tom linie. *)
fun readEmptyLine socket =
let 
    fun read () =
    let val line = readLine socket
        fun empty l = (l = "") orelse (l = "\n") orelse (l = "\r\n")
    in  if empty line then line else line ^ read() 
    end
in  read() end; 

(* unix: unit -> bool

   Returnerer sand, hvis maskinen er unix-variant.
   Det udnyttes, at unix-maskiner IKKE har drevbogstaver ligesom
   MS-operativsystemerne. *)
fun unix () = not (Path.validVolume {isAbs = true, vol = "C:\\"});

val regstr_ip = "([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+)";
val regexp_ip = Regex.regcomp regstr_ip [Regex.Extended, Regex.Icase];
    
(* numericAddress: string -> bool 

   Returnerer sand, hvis strengen har form af en ip-adresse
   fx. 255.0.0.1 *)
fun numericAddress addr =
let val match = Regex.regexec regexp_ip [] addr
in  case match of NONE   => false
                | SOME _ => true
end;

(* readFile: string -> string

   Indlæser hele indholdet af en fil med filnavnet fnvn. *)
fun readFile fnvn = 
let val indstrm = TextIO.openIn fnvn
in  TextIO.inputAll indstrm before 
    TextIO.closeIn(indstrm)
end;

(* tmpName: unit -> string

   tmpName vil lave et midlertidigt filnavn udfra 
   klokkeslet *)
val begin = Time.now()
fun tmpName () = 
let val t = Time.-(Time.now(), begin)
    val secs  = Time.toSeconds t
    val msecs = trunc(((Time.toReal t) - (real secs)) * 1000.0)
    val num = 1000 * (secs mod 1000) + (msecs mod 1000)
    val str = Int.toString num
in  str ^ ".tmp" end;

(* safeRemove: string -> unit

   safeRemove fjerner filen, men melder ikke fejl,
   hvis det gik galt. *)
fun safeRemove filename = (FileSys.remove filename)
                          handle _ => ();

(* run: string -> string option

   Kører program og returnerer SOME stdout, eller NONE
   ved fejl. Mosml-modulet har en tilsvarende funktion,
   som desværre ikke synes at virke under win32. 

   ADD: At default this code didn't take advantage of the Unix module
   as it does in gethostbyname so we added so it calls the command
   directly by Unix.execute and returns the output back in a stream
   instead of saving the output in a lame file and then reading it
   back an deleting it *) 
fun run line =
    let
        fun runWindows line = 
            let val temp = tmpName()
                val command = line ^ " > " ^ temp
                val status = Process.system command
            in  (if status = Process.failure then NONE 
                 else SOME (readFile temp)) 
                before safeRemove temp
            end;
            
        fun runUnix line = 
            SOME (
            TextIO.inputAll(
            #1 (Unix.streamsOf
                    (Unix.execute("/bin/sh" , ["-c", line]))))) 
            handle Fail s => NONE
    in
        if unix() then
            runUnix line
        else
            runWindows line
            
    end;
       
(* gethostbyname: string -> string

   gethostbyname vil forsøge at finde ip-adresse ud fra
   hostnavn. Først forsøges selve Socket-modulet; hvis 
   dette fejler, kaldes ping, som findes både på unix og
   win32, og resultatet trækkes ud herfra. Hvis alt går galt,
   kastes en Fail-undtagelse. *)

fun gethostbyname name = if numericAddress name then name else
let fun trySocket () = 
   (let val ip = Socket.getinetaddr (Socket.inetAddr name 80)
    in if ip = "0.0.0.0" orelse 
           ip = "255.255.255.255" then NONE
                                  else SOME ip
    end) handle Fail _ => NONE
    fun tryPing () = 
    let val host = if unix() then "host " ^ name
                   else "ping -n 1 " ^ name
        val status = run host
        val output = case status of SOME str => str 
                                  | NONE     => ""
        val match = Regex.regexec regexp_ip [] output
        val vector = valOf match
        val sub = Vector.sub(vector, 1)
        val str = Substring.string sub
    in  SOME str end 
    handle _ => NONE
in  case trySocket() of SOME ip => ip | NONE =>
    ( case tryPing() of SOME ip => ip | NONE => 
      raise Fail ("host lookup failed: " ^ name)
    )
end;

fun assoc x list = case List.find (fn (y, _) => x = y) list
                    of SOME (_, value) => SOME (value)
                     | NONE => NONE;

(* Basic IO stuff *)
exception IOError of string;

fun readFrom fileName = 
    let
      val inStrm = (TextIO.openIn fileName) handle _ => raise IOError fileName
    in
        TextIO.inputAll inStrm
    end


(* Some usefull string functions *)

(* returns the index of the first occurence of chr in str *)
fun getFirstIndexOf (chr, str) = 
    let
        fun getFirstIndexOf' chr index "" = index
          | getFirstIndexOf' chr index str =
            if (String.size str) <   index then
                if (String.sub (str, index) = chr) then
                    index
                else
                    getFirstIndexOf' chr (index+1) str
            else
                (* if we checked the hole string and didnt find anything then return 0 *)
                0
    in
        getFirstIndexOf' chr 0 str 
    end

(* trims a string from spaces in front and back *)
fun trimStr str = 
    (* If trimStr gets a empty string or string of spaces then
       String.tokens returns a empty list. To deal with this we have
       the case to check for empty list and return empty string *)
    case (String.tokens Char.isSpace str) of
        [] => ""
      (* if it aint a empty list then the first element in the list is
         the string of interes *)
      | strLst => List.hd strLst

end;
          
