signature Config =
sig
    (* Return the HTTP user agent ID used by the application. *)
    val HttpUserAgent : unit -> string;
end
