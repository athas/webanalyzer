signature Http =
sig 
    eqtype URI
    exception badURI
    (* Ikke-kanoniseret *)
    val buildSimpleURI : URI option * string -> URI
    (* Kanoniseret *)
    val buildURI : URI option * string -> URI

    (* Streng-repræsentation af uri. Det vil gælde for alle uri, at: 
         buildURI(NONE, stringFromURI uri) = uri *)
    val stringFromURI : URI -> string

    val protocolFromURI : URI -> string 
    val serverFromURI : URI -> string
    val pathFromURI : URI -> string
    val contentTypeFromURI : URI -> string

   (* Failure kan indeholde tre forskellige 
      fejlmeddelelser. HTTP har en status-kode 
      defineret i http-protokollen og dernæst en
      forklarende tekstbesked. Socket bruges, hvis 
      et problem er opstået med netværkskommunikationen. 
      Endelig vil alle andre fejlmeldinger være 
      dækket af General. *)
    datatype Failure = HTTP of int * string
                     | Socket of string
                     | General of string
    exception Error of Failure

    (* Get the error message of a Failure. *)
    val failString : Failure -> string

   (* Henter html-fil, som URI refererer til. En Error-undtagelse
      rejses ved alle slags problemer. *)
    val getURI : URI -> string
end;
