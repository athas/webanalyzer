signature HTMLLexer =
sig

    type tag;
    type text;

    datatype lexeme = StartTagLexeme of tag
                    | EndTagLexeme of tag
                    | TextLexeme of text;

    val tagName : tag -> string;
    val getAttribute : string -> tag -> string option;
    val 'b mapAttributes : (string * string -> 'b) -> tag -> 'b list;

    val textContents : text -> string;
    
    val lex : string -> lexeme list;

end
