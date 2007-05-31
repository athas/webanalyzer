structure HTMLFilter :> HTMLFilter =
struct

open HTMLParser;

datatype Filter = ByTagName of string
                | ByAttribute of (string * string)
                | And of Filter * Filter
                | Or of Filter * Filter
                | Not of Filter;

infix And;
infix Or;

(* remove: Filter -> tag -> bool
   Does the filter say that the tag should be removed? *)
fun evaluate _ (Text _) = false
  | evaluate (ByTagName tn) (Tag (tag,_)) = (tn = tagName tag)
  | evaluate (ByAttribute (name, value)) (Tag (tag,_)) =
                         (case getAttribute name tag of
                             NONE => false
                           | SOME v => value = v)
  | evaluate (f1 And f2) tag = evaluate f1 tag andalso
                                  evaluate f2 tag
  | evaluate (f1 Or f2) tag = evaluate f1 tag orelse
                                 evaluate f2 tag
  | evaluate (Not f) tag = evaluate f tag;



(* Filter -> parsetree list -> parsetree list *)
fun filterhtml filter tree = HTMLParser.filter (evaluate filter) tree;

val standardfilter =  (Not (ByTagName "body"))
                          And (Not (ByTagName "head"))
                          And (Not (ByTagName "html"))
                          And (ByTagName "script")
                          And (ByTagName "style")
                          And (ByTagName "colgroup")
                          And (ByTagName "col");


end;
