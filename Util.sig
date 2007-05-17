signature Util =
sig type 'a sock = ('a, Socket.active Socket.stream) Socket.sock

    val readChar: 'a sock -> char
    val readLine: 'a sock -> string
    val readEmptyLine: 'a sock -> string

    (* Oversætter symbolsk navn på vært til ip-adresse *)
    val gethostbyname: string -> string
end;
