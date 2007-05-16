(* The Lexer generates Lexeme values which are then fed to the
parser. This is the definition of the lexeme type.
*)

structure HTMLLexeme =
struct

type attributelist = (string * string) list;
fun getAttribute attrname ((name, value) :: rest) =
    if name = attrname then SOME(value)
    else getAttribute name rest
  | getAttribute name [] = NONE;
fun mapAttributes f attributes = map f attributes;

type tag = string * attributelist;
fun getTagName ((tagname, _) : tag) = tagname;     
fun getTagAttributes ((_, attributes) : tag) = attributes;

type text = string;
fun getText (text : text) = text;

datatype lexeme = StartTagLexeme of tag
                | EndTagLexeme of tag
                | TextLexeme of text;
end
