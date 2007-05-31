structure SpellChecker :> SpellChecker =
struct

(* GNU Aspell-based implementation, keeping processes alive between
function calls. *)

open Unix;
open Util;
open List;
open TextIO;

exception dictionaryNotFound of string;
exception badWord of string;
exception badLanguageCode of string;
(* Thrown if aspell cannot be started. *)
exception aspellNotFound;

(* Mapping language codes to aspell process streams for great justice. *)
val aspellProcesses : (string * (instream, outstream) Unix.proc) list ref = ref [];

(* Return true if okay, false if it's bad. *)
fun checkLanguageCode languageCode = size languageCode = 2 andalso
                                     all Char.isAlpha (explode languageCode);

(* Return true if aspell is available, false otherwise. *)
fun hasAspell _ = not (null (!aspellProcesses)) orelse
                  isSome (run ("aspell -v"));

(* Start an aspell process spellchecking words in the provided language. *)
fun startAspell languageCode = 
    if not (hasAspell ())
    then raise aspellNotFound
    else if checkLanguageCode languageCode
    then let val pr = execute ("/bin/sh" , ["-c", "aspell -d " ^ languageCode ^ " pipe --encoding iso8859-1"])
             val (instream, _) = streamsOf pr
             (* Read the greeting line. *)
             val _ = inputLine instream
         in aspellProcesses := ((languageCode, pr) :: (!aspellProcesses)) end
    else raise badLanguageCode languageCode;

(* Find a running aspell process for the provided
language. Returns NONE if no such process is running. *)
fun findAspellProcess languageCode = case find (fn (lang, _) => lang = languageCode) (!aspellProcesses) of
                                         SOME (_, process) => SOME process
                                       | NONE => NONE

(* Return an aspell process for the provided language, starting a new
process if none is already running. *)
fun ensureAspellProcess languageCode = case findAspellProcess languageCode of
                                           SOME process => process
                                         | NONE => (startAspell languageCode;
                                                    ensureAspellProcess languageCode)

fun hasDictionary languageCode = if exists (fn (lang, _) => lang = languageCode) (!aspellProcesses)
                                 then true 
                                 else if not (checkLanguageCode languageCode)
                                 then raise badLanguageCode languageCode 
                                 else case run("aspell dicts") of 
                                          SOME result => member (languageCode, (splitLines result))
                                        | NONE => false;

fun spellCheckWord languageCode word = 
    if not (hasDictionary languageCode) 
    then raise dictionaryNotFound languageCode
    else if exists (equal #"\"") (explode word)
    then raise badWord word
    else let val process = ensureAspellProcess languageCode 
             (* Apparently, SML/NJ will close these streams for us. In
              any case, death and carnage comes to any who attempts to
              close them. *)
             val (instream, outstream) = streamsOf process in
             output (outstream, word ^ "\n");
             flushOut outstream;
             (case inputLine instream of
                  SOME resultLine => (inputLine instream;
                                      size resultLine = 0 orelse
                                      String.sub (resultLine, 0) = #"*" orelse
                                      String.sub (resultLine, 0) = #"+" orelse
                                      String.sub (resultLine, 0) = #"-")
                | NONE => false)
         end handle aspellNotFound => false;

fun spellCheckWords languageCode words = 
    map (fn word => (word, spellCheckWord languageCode word)) words;

end
