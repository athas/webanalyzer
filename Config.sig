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

    val setOutputDir: string -> unit;
    val outputDir: unit -> string option;

    val fractionalDigits : int;

    val isVowel : char -> bool;
    val isAlphabetic : char -> bool;

    val toggleLix: unit -> unit;
    val lix: unit -> bool;
    val toggleFre: unit -> unit;
    val fre: unit -> bool;
    val toggleFkgl: unit -> unit;
    val fkgl: unit -> bool;
    val toggleSpell: unit -> unit;
    val spell: unit -> bool;
    val toggleFindRepetitions: unit -> unit;
    val findRepetitions: unit -> bool;

    val setDefaults: unit -> unit;


    val addTagNameFilter : string -> unit;
    val tagNameFilters : unit -> string list;
    val addIdFilter : string -> unit;
    val idFilters : unit -> string list;
end
