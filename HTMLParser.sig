signature HTMLParser =
sig

    type tag;
    type text;
         
    datatype parsetree = Tag of tag * parsetree list
                       | Text of text;
             
    val tagName : tag -> string;
    val getAttribute : string -> tag -> string option;
    val 'b mapAttributes : (string * string -> 'b) -> tag -> 'b list;

    val textContents : text -> string;

    val parse : string -> parsetree list;

end
