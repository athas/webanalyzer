signature Config =
sig
    (* Return the HTTP user agent ID used by the application. *)
    val setHttpUserAgent: string -> unit;
    val httpUserAgent : unit -> string;
    val setCrawlDepthLimit : int -> unit;
    val crawlDepthLimit : unit -> int;
    val setCrawlDelay: int -> unit;
    val crawlDelay: unit -> int;

    val setDefault: unit -> unit;
end
