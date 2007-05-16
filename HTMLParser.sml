structure HTMLParser :> HTMLParser =

struct
open HTMLLexeme;
open HTMLParseTree;

(* Given a string containing fairly valid HTML, return a parse tree of
the HTML. *)
fun parsefun (StartTagLexeme (tag, attributes) :: lexemes) tagstack = 
    let val (children, childrest) = parsefun lexemes (tag :: tagstack);
        val (successors, succrest) = parsefun childrest tagstack
    in (Tag ((tag, attributes), children) :: successors, succrest) end
  | parsefun (EndTagLexeme (tag, attributes) :: lexemes) (headtag :: tagstack) =
    if tag = headtag then ([], lexemes)
    else if List.exists (fn y => tag = y) tagstack then ([], EndTagLexeme (tag, attributes) :: lexemes)
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
                   in worker ([], (HTMLLexer.lex string)) end;

end
