(* Util indeholder netværksfunktionalitet der bruges af
   server.sml og Http modulet. ander@diku.dk *) 
structure Util :> Util =
struct

local
    open RegexMatcher;
    fun matches regex string = StringCvt.scanString (find regex) string

                               
    (* Return the actual match as element #1 in the list and there after
       the sub-matches (defined with paranteses in the regexp) *)
    fun matchList' string (MatchTree.Match (SOME {pos, len}, subtrees)) =
        String.substring (string, pos, len) :: (matchList'' string subtrees)
      | matchList' string (MatchTree.Match (NONE, subtrees)) = "" :: (matchList'' string subtrees)

    and matchList'' string matchtreelist =
        (foldl (fn (m, b) => b @ matchList' string m) [] matchtreelist)
in
    fun isMatch regex string = case matches regex string of
                                   SOME x => true
                                 | NONE => false;

    fun firstMatch regex string =
            case matches regex string of
                SOME (MatchTree.Match (SOME {pos, len}, _)) =>
                        SOME (String.substring (string, pos, len))
              | _ => NONE;
       
    fun matchList regex string = 
        case matches regex string of
            SOME x => SOME (matchList' string x)
          | _ => NONE;

    fun regexTokens regex str = 
        let
            fun regexTokens' ret _ "" = ret
              | regexTokens' ret regex str = 
                let
                    val match = matches regex str
                in
                    case match of 
                        SOME (MatchTree.Match (SOME {len, pos}, _)) => 
                        let
                            val beforeMatch = String.substring(str, 0, pos)
                            val afterMatch = String.substring(str,(pos+len), 
                                                              ((String.size str)-(pos+len)))
                        in
                            regexTokens' (beforeMatch::ret) regex afterMatch
                        end
                      | _ => str :: ret
                end
            
        in
            regexTokens' [] regex str
        end;

end
        

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

(* run: string -> string option

   Kører program og returnerer SOME stdout, eller NONE
   ved fejl.

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
        if Unix.reap pr = OS.Process.success
        then SOME(result)
        else NONE
    end handle Fail _ => NONE;
       
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
            handle IO.Io _ => raise IOError fileName
    in TextIO.output (ostream, str) before
       TextIO.closeOut ostream end

(* Some usefull string functions *)

val strToLower = String.translate (str o Char.toLower);

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

val equalICase = (equal o strToLower o strToLower);

(* x member y: ''a * ''a list -> bool

   Does x occur in the list y? *)
infix 0 member;
fun x member y = List.exists (equal x) y;


(* concatMap func lst: ('a -> 'b list) -> 'a list -> 'b list

   Maps func over the elements in lst, concatenating the results. *)
fun concatMap f l = (foldr (op @) []) (map f l);

(* SOMEs : 'a option list -> 'a list
 
   Takes a list of options and returns all the SOME-values. *)
fun SOMEs x = map Option.valOf (List.filter Option.isSome x);

val splitLines = String.fields (equal #"\n");

fun wait sec = ();

fun wait (seconds : int) =
    let
        open Time;
        open LargeInt;
        val endTime = Time.+(now (), fromSeconds (Int.toLarge seconds))
        fun wait' () = if Time.>=(now (), endTime) then ()
                       else wait' ()
    in
        wait' ()
    end;

fun time f arg =
    let val beginTime = Time.now () in
        (f arg) before 
        let val endTime = Time.now () 
            val timeDiff = Time.-(endTime, beginTime) in
            print (Time.toString timeDiff);
            print " seconds.\n" end
    end;


(* Format a real for printing, rounds to the amount of digits defined
   in Config.fractionalDigits and uses "-" instead of "~". *)
fun formatForOutput x =
    let
        val sign = if x < 0.0 then "-" else "";
        val rounded = Real.fmt (StringCvt.FIX (SOME Config.fractionalDigits))
                               (Real.abs x);
    in
        sign ^ rounded
    end;
end;

