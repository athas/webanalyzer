structure SpellChecker :> SpellChecker =
struct

(* GNU Aspell-based implementation. *)

open Util;
open List;

exception dictionaryNotFound of string;
exception badWord of string;
exception badLanguageCode of string;

(* Return true if okay, false if it's bad. *)
fun checkLanguageCode languageCode = size languageCode = 2 andalso
                                     all Char.isAlpha (explode languageCode);

fun hasDictionary languageCode = if not (checkLanguageCode languageCode)
                                 then raise badLanguageCode languageCode 
                                 else case run("aspell dicts")
                                       of SOME result => member (languageCode, (splitLines result))
                                        | NONE => false;

fun spellCheckWord languageCode word = 
    if not (hasDictionary languageCode) 
    then raise dictionaryNotFound languageCode
    else if exists (equal #"\"") (explode word)
    then raise badWord word
    else case run ("echo \"" ^ word ^ "\" | aspell -l \"" ^ languageCode ^ "\" pipe --encoding iso8859-1") of
             SOME result => let val lines = splitLines result
                                val resultLine = if List.length lines = 0 then ""
                                                 else List.nth (lines, 1)
                            in size resultLine = 0 orelse
                               String.sub (resultLine, 0) = #"*" orelse
                               String.sub (resultLine, 0) = #"+" orelse
                               String.sub (resultLine, 0) = #"-"
                            end
           | NONE => false;

fun spellCheckWords languageCode words = 
    map (fn word => (word, spellCheckWord languageCode word)) words;

end
