signature HTMLParser =
sig

    type tag;
    type text;
         
    datatype parsetree = Tag of tag * parsetree list
                       | Text of text;

    (* Returns a string containing the tagname *)
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

    (* Converts types of text to a string *)
    val textContents : text -> string;

    (* Makes a parsetree list of a given string *)
    val parse : string -> parsetree list;

    (* Finds a subtree in the given parsetree list, given its tagname *)
    val find : string -> parsetree list -> parsetree option;

    (* Removes the subtrees that violates the predicate. *)
    val filter :  (parsetree -> bool) -> parsetree list -> parsetree list;

end
