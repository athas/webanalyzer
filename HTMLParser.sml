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
    let val tagName = (HTMLLexer.tagName tag);
        val (children, childrest) = findChildren tagName lexemes tagstack;
        val (successors, succrest) = parsefun childrest tagstack;
    in (Tag (tag, children) :: successors, succrest) end
  | parsefun (EndTagLexeme tag :: lexemes) (headtag :: tagstack) =
    if HTMLLexer.tagName tag = headtag then ([], lexemes)
    else if List.exists (fn x => HTMLLexer.tagName tag = x) tagstack then ([], EndTagLexeme tag :: lexemes)
    else parsefun lexemes tagstack
  | parsefun (EndTagLexeme _ :: lexemes) [] = parsefun lexemes []
  | parsefun (TextLexeme text :: lexemes) stacktop =
    let val (successors, lexemerest) = parsefun lexemes stacktop
    in (Text text :: successors, lexemerest) end
  | parsefun _ [] = ([], [])
  | parsefun [] _ = ([], [])
and findChildren "br" lexemes tagstack = ([], lexemes)
  | findChildren "hr" lexemes tagstack = ([], lexemes)
  | findChildren parentName lexemes tagstack = 
    parsefun lexemes (parentName :: tagstack);

fun parse string = let fun worker (tree, []) = tree
                         | worker (tree, lexemes) =
                           tree @ (worker o parsefun lexemes) []
                   in worker ([], (lex string)) end;

(* findElement : string -> parsetree list -> parsetree option

   Finds the first subtree in lst with tag-name y.
   Only searches the top level of given the parsetree list. *)
fun find y [] = NONE
  | find y ((x as (Tag (tag, subtrees))) :: xs) =
        if y = tagName tag
        then SOME x
        else (case find y subtrees of
                 SOME z => SOME z
               | NONE => find y xs)
  | find y (_ :: xs) = find y xs;

end
