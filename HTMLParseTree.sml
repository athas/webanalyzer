structure HTMLParseTree =
struct

type tag = string * (string * string) list
datatype parsetree = Tag of tag * parsetree list
                   | Text of string;
    
end
