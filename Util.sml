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
        val pr = Unix.execute("/bin/sh" , ["-c", line])
        val result = TextIO.inputAll(#1 (Unix.streamsOf pr))
    in
        if Unix.reap pr = Process.success
        then SOME(result)
        else NONE
    end handle Fail _ => NONE;
       
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

fun writeTo fileName str =
    let val ostream = TextIO.openOut fileName
            handle Io _ => raise IOError fileName
    in TextIO.output (ostream, str) before
       TextIO.closeOut ostream end

(* Some usefull string functions *)

(* returns the index of the first occurence of chr in str *)
fun getFirstIndexOf (chr, str) = 
    let
        fun getFirstIndexOf' chr index "" = index
          | getFirstIndexOf' chr index str =
            if index < (String.size str) then
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

(* Return the index of the last occurence of char in
   string. Returns size of string if char is not in string. *)
fun getLastIndexOf (char, string) =
    let
        fun getLastIndexOf' index = if index < 0
                                    then size string
                                    else if String.sub (string, index) = char
                                    then index
                                    else getLastIndexOf' (index - 1)
    in
        getLastIndexOf' (size string - 1)
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

(* equal : ''a -> ''a -> bool

   A currying equivalence function.*)

fun equal x y = x = y;

(* x member y: ''a * ''a list -> bool

   Does x occur in the list y? *)
infix 0 member;
fun x member y = List.exists (fn z => z = x) y;


(* concatMap func lst: ('a -> 'b list) -> 'a list -> 'b list

   Maps func over the elements in lst, concatenating the results. *)
fun concatMap f l = (foldr (op @) []) (map f l);

(* SOMEs : 'a option list -> 'a list
 
   Takes a list of options and returns all the SOME-values. *)
val SOMEs = (map Option.valOf) o (List.filter Option.isSome);

fun wait seconds =
    let
        open Time;
        val endTime = now () + fromSeconds seconds
        fun wait' () = if now () >= endTime then ()
                       else wait' ()
    in
        wait' ()
    end;

end;
