signature Config =
sig
    (* Return the HTTP user agent ID used by the application. *)
    val setHttpUserAgent: string -> unit;
    val httpUserAgent : unit -> string;
    val setCrawlDepthLimit : int -> unit;
    val crawlDepthLimit : unit -> int;
    val setCrawlDelay: int -> unit;
    val crawlDelay: unit -> int;
    (* Set the default language web-pages are assumed to be in. The
    argument must be a two-letter language code. *)
    val setDefaultLanguage: string -> unit;
    (* Return the default language web-pages are assumed to be in. The
    return value is a two-letter language code. *)
    val defaultLanguage: unit -> string;

    val fractionalDigits : int;

    val isVowel : char -> bool;
    val isAlphabetic : char -> bool;

    val setLix: unit -> unit;
    val lix: unit -> bool;
    val setFre: unit -> unit;
    val fre: unit -> bool;
    val setFkgl: unit -> unit;
    val fkgl: unit -> bool;
    val setSpell: unit -> unit;
    val spell: unit -> bool;

    val setDefaults: unit -> unit;
end
