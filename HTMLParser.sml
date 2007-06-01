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
        (* Find a matching close tag, and consider all lexemes before
        it as children of this tag.  *)
        val (children, childrest) = findChildren tagName lexemes tagstack;
        val (successors, succrest) = parsefun childrest tagstack;
    in (Tag (tag, children) :: successors, succrest) end
  | parsefun (EndTagLexeme tag :: lexemes) (headtag :: tagstack) =
    (* If this closing tag matches the most recently opened tag, that
    means we are done finding children for it. *)
    if HTMLLexer.tagName tag = headtag then ([], lexemes)
    (* If the closing tag matches some opened tag, assume that we are
    missing a closing tag (BAD user!!! Write well-formed HTML
    dammit!), and just assume that we read the proper closing tag, and
    try reading the current closing tag again. *)
    else if List.exists (fn x => HTMLLexer.tagName tag = x) tagstack
    then ([], EndTagLexeme tag :: lexemes)
    (* Closing a tag that isn't open? Let's just pretend this never
    happened. *)
    else parsefun lexemes tagstack
  | parsefun (EndTagLexeme _ :: lexemes) [] = 
    (* Closing a tag, but there are no open tags. Again, this never happened. *)
    parsefun lexemes []
  | parsefun (TextLexeme text :: lexemes) stacktop =
    (* A text lexeme is simple, it has no children, only successors. *)
    let val (successors, lexemerest) = parsefun lexemes stacktop
    in (Text text :: successors, lexemerest) end
  | parsefun _ [] = ([], [])
  | parsefun [] _ = ([], [])
(* Some tags never have children, despite not necessarily being
closed. Handle them here. *)
and findChildren "br" lexemes tagstack = ([], lexemes)
  | findChildren "hr" lexemes tagstack = ([], lexemes)
  | findChildren parentName lexemes tagstack = 
    parsefun lexemes (parentName :: tagstack);

(* parse : string -> parsetree list *)
fun parse string = 
    (* Make a list of lexemes, then accumulate parse trees. *)
    let fun worker (tree, []) = tree
          | worker (tree, lexemes) =
            tree @ (worker o parsefun lexemes) []
    in worker ([], (lex string)) end;

(* findElement : string -> parsetree list -> parsetree option

   Finds the first subtree in lst with tag-name y.
   Uses depth-first search *)
fun find y [] = NONE
  | find y ((x as (Tag (tag, subtrees))) :: xs) =
        if y = tagName tag
        then SOME x
        else (case find y subtrees of
                 SOME z => SOME z
               | NONE => find y xs)
  | find y (_ :: xs) = find y xs;

(* filter : (parsetree -> bool) -> parsetree list -> parsetree list
   Removes the subtrees that violates the predicate. *)
fun filter p [] = []
  | filter p ((x as (Text _)) :: xs) = x :: (filter p xs)
  | filter p ((x as (Tag (tag, subtrees))) :: xs) =
          if p x
          then Tag (tag, filter p subtrees)
               :: filter p xs
          else filter p xs;

fun mapLinks function htmlTree =
    let
        (* takes a parsetree list and processes down it by calling
           mapLinks' on the children of the tree *)
        fun getChildren' (ret, []) = ret
          | getChildren' (ret, (child :: rest)) =
            getLinks' (ret, child) @ getChildren' ([], rest)
            
        (* Checks each parsetree for Tag's and if the tags are what
           we looks for then it scans the tags known link attributes
           and adds then return all these links. 
           'AND' because these to functions are mutually recursive *)
        and getLinks' (ret, Text t) = ret
          | getLinks' (ret, Tag (tag, [])) = ret
          | getLinks' (ret, Tag (tag, children)) =     
            case tagName tag of
                "a"      => function tag :: getChildren' (ret, children)
              | "frame"  => function tag :: getChildren' (ret, children)
              | "iframe" => function tag :: getChildren' (ret, children)
              | "img"    => function tag :: getChildren' (ret, children)
              | "ins"    => function tag :: getChildren' (ret, children)
              | "del"    => function tag :: getChildren' (ret, children)
              (* If none of the above cases just move on to next tag in the parsetree *)
              | _ => getChildren' (ret, children)
    in
        getChildren'([], htmlTree)
    end;

end
