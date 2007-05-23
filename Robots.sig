signature Robots =
sig
    
    val initRobotsTxt: string -> unit;
    val clearRobotsTxt: unit -> unit;
    val getDisallowedPaths: unit -> string list;
    val isPathAllowed: string -> bool;
    val setCrawlDelay: int -> unit;
    val getCrawlDelay: unit -> int;
                                 
end;
