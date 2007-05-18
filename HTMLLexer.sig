signature HTMLLexer =
sig
    (* A tag has a name and a set of attributes (string keys mapping
    to string values) *)
    type tag;
         
    (* Anything that is not a tag, can be seen as a string. *)
    type text;

    datatype lexeme = StartTagLexeme of tag
                    | EndTagLexeme of tag
                    | TextLexeme of text;

    (* Return the name of a tag in lowercase. *)
    val tagName : tag -> string;

    (* Return the attribute of a tag (second argument) with the
    desired name (first argument). The attribute name should be in
    lowercase, as all attribute names are converted to lowercase
    during lexing. *)
    val getAttribute : string -> tag -> string option;

    (* Map across all the attributes in a tag (second argument) in an
    unspecified order. For each attribute, a function taking a tuple
    as argument (first argument) will be called, and a list containing
    the return values of these calls will be returned. *)
    val mapAttributes : (string * string -> 'b) -> tag -> 'b list;

    (* Return the contents of a text lexeme as a string. *)
    val textContents : text -> string;

    (* Lex the provided string, returning a list of lexemes. *)
    val lex : string -> lexeme list;

end
