signature Util =
sig type 'a sock = ('a, Socket.active Socket.stream) Socket.sock

    val readChar: 'a sock -> char
    val readLine: 'a sock -> string
    val readEmptyLine: 'a sock -> string

    (* Oversætter symbolsk navn på vært til ip-adresse *)
    val gethostbyname: string -> string

    val assoc: ''a -> (''a * 'b) list -> 'b option

    val readFrom: string -> string
    val writeTo: string -> string -> unit

    (* Return the index of the first occurence of char in
    string. Returns 0 if char is not in string. *)
    val getFirstIndexOf: char * string -> int

    (* Return the index of the last occurence of char in
    string. Returns size of string if char is not in string. *)
    val getLastIndexOf: char * string -> int

    val trimStr: string -> string

    exception IOError of string

    val member : ''a * ''a list -> bool
    val concatMap : ('a -> 'b list) -> 'a list -> 'b list;
    val SOMEs : 'a option list -> 'a list

    (* Wait for the provided amount of seconds before returning. *)
    val wait : int -> unit
end;
