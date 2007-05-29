signature Util =
sig type 'a sock = ('a, Socket.active Socket.stream) Socket.sock

    val isMatch : RegexMatcher.regexp -> string -> bool;
    val firstMatch : RegexMatcher.regexp -> string -> string option;
    val matchList : RegexMatcher.regexp -> string -> string list option;

    val readChar: 'a sock -> char
    val readLine: 'a sock -> string
    val readEmptyLine: 'a sock -> string

    val run : string -> string option

    (* Oversætter symbolsk navn på vært til ip-adresse *)
    val gethostbyname: string -> string

    val assoc: ''a -> (''a * 'b) list -> 'b option

    val readFrom: string -> string
    val writeTo: string -> string -> unit

    val strToLower: string -> string
    (* Return the index of the first occurence of char in
    string. Returns 0 if char is not in string. *)
    val getFirstIndexOf: char * string -> int

    (* Return the index of the last occurence of char in
    string. Returns size of string if char is not in string. *)
    val getLastIndexOf: char * string -> int

    val trimStr: string -> string

    exception IOError of string

    val equal : ''a -> ''a -> bool
    val equalICase: string -> string -> bool
    val member : ''a * ''a list -> bool
    val concatMap : ('a -> 'b list) -> 'a list -> 'b list
    val SOMEs : 'a option list -> 'a list
    (* Split a string containing newline characters into a list of lines. *)
    val splitLines : string -> string list

    (* Wait for the provided amount of seconds before returning. *)
    val wait : int -> unit

    (* Format a real for printing, rounds to the amount of digits defined
       in Config.fractionalDigits and uses "-" instead of ugly "~". *)
    val formatForOutput : real -> string;

    (* Print how long it takes a function of a single argument to execute. *)
    val time : ('a -> 'b) -> 'a -> 'b;
end;
