structure SpellChecker :> SpellChecker =
struct

(* GNU Aspell-based implementation. *)

open Util;
open List;

exception dictionaryNotFound of string;

fun hasDictionary languageCode = case run("aspell dicts")
                                  of SOME result => member (languageCode, (splitLines result))
                                   | NONE => false;

fun spellCheckWord languageCode word = 
    if not (hasDictionary languageCode) 
    then raise dictionaryNotFound languageCode
    else case run ("echo \"" ^ word ^ "\" | aspell -l \"" ^ languageCode ^ "\" pipe --encoding iso8859-1") of
             SOME result => String.sub (List.nth (splitLines result, 1), 0) = #"*"
           | NONE => false;

fun spellCheckWords languageCode words = 
    map (fn word => (word, spellCheckWord languageCode word)) words;

end
