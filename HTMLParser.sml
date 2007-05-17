structure HTMLParser :> HTMLParser =

struct
open HTMLLexer;

type tag = HTMLLexer.tag;
type text = HTMLLexer.text;

datatype parsetree = Tag of tag * parsetree list
                   | Text of text;

val tagName = HTMLLexer.tagName;
val getAttribute = HTMLLexer.getAttribute;
val mapAttributes = HTMLLexer.mapAttributes;

val textContents = HTMLLexer.textContents;

fun tagAttributes tag = mapAttributes (fn tuple => tuple) tag;

(* Given a string containing fairly valid HTML, return a parse tree of
the HTML. *)
fun parsefun (StartTagLexeme tag :: lexemes) tagstack = 
    let val (children, childrest) = parsefun lexemes (HTMLLexer.tagName tag :: tagstack);
        val (successors, succrest) = parsefun childrest tagstack
    in (Tag (tag, children) :: successors, succrest) end
  | parsefun (EndTagLexeme tag :: lexemes) (headtag :: tagstack) =
    if HTMLLexer.tagName tag = headtag then ([], lexemes)
    else if List.exists (fn y => HTMLLexer.tagName tag = y) tagstack then ([], EndTagLexeme tag :: lexemes)
    else parsefun lexemes tagstack
  | parsefun (EndTagLexeme _ :: lexemes) [] = parsefun lexemes []
  | parsefun (TextLexeme text :: lexemes) stacktop =
    let val (successors, lexemerest) = parsefun lexemes stacktop
    in (Text text :: successors, lexemerest) end
  | parsefun _ [] = ([], [])
  | parsefun [] _ = ([], []);

fun parse string = let fun worker (tree, []) = tree
                         | worker (tree, lexemes) =
                           tree @ (worker o parsefun lexemes) []
                   in worker ([], (lex string)) end;

end
