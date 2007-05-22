signature HTMLParser =
sig

    type tag;
    type text;
         
    datatype parsetree = Tag of tag * parsetree list
                       | Text of text;

    (* Returns a string containing the tagname *)
    val tagName : tag -> string;

    (* 
       Returns 'SOME attribute_data' or 'NONE' if the attribute exists in the given tag.
       string = HTML tag attribute name in lowercase to find
       tag = HTML tag from parsetree to search in 
       
     *)
    val getAttribute : string -> tag -> string option;
    
    (* 
       Like List.map. It takes a function ex. (fn key, value => ....) on
       a given tag and maps that function on all attributes and returns
       the modifyed list 
     *)
    val mapAttributes : (string * string -> 'b) -> tag -> 'b list;

    (* Converts types of text to a string *)
    val textContents : text -> string;

    (* Makes a parsetree list of a given string *)
    val parse : string -> parsetree list;

    (* Finds a subtree in the given parsetree list, given its tagname *)
    val find : string -> parsetree list -> parsetree option

end
