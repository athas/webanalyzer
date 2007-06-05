signature SpellChecker =
sig

    (* Thrown when asked to check the spelling for an unknown
    language. The string is the language code that was not known. *)
    exception dictionaryNotFound of string;

    (* Thrown when asked to spellcheck a word that contains
    troublesome non-word-characters. *)
    exception badWord of string;

    (* Thrown when asked to use a language code that does not appear
    valid. A valid language code contains two characters. *)
    exception badLanguageCode of string;

    (* Given a language code and a word, return a boolean indicating
    whether the word is spelled properly according to the language. *)
    val spellCheckWord : string -> string -> bool;

    (* Given a language code and a list of words, return a list
    mapping words to boolean values indicating whether they are
    properly spelled. The result list will have the same sequence of
    words as the input list. The result is equivalent to:
    map (fn w => (w, spellCheckWord language w)) words
    , but may be faster. *)
    val spellCheckWords : string -> string list -> (string * bool) list;

    (* Return true if we are able to spell-check for the given language code. *)
    val hasDictionary : string -> bool;

    (* Return a boolean value indicating whether spell-checking is
    available at all. If this return value is false, all calls to
    spellCheckWord will return true. *)
    val spellCheckingAvailable : unit -> bool;

end
