signature HTMLFilter =
sig
    
    datatype Filter = ByTagName of string
                    | ByAttributes of (string * string) list
                    | Combination of string * ((string * string) list);

    (* removes unwanted content from a HTML parsetree *)
    val filterhtml : Filter list
                     -> HTMLParser.parsetree list
                     -> HTMLParser.parsetree list;

    (* Removes script and style tags *)
    val standardfilter : Filter list;


end
