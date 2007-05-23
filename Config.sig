signature Config =
sig
    (* Return the HTTP user agent ID used by the application. *)
    val HttpUserAgent : unit -> string;

    val setCrawlDepthLimit : int -> unit;
    val crawlDepthLimit : unit -> int;
end
