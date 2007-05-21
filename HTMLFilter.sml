structure HTMLFilter :> HTMLFilter =
struct

open HTMLParser;

datatype Filter = ByTagName of string
                | ByAttributes of (string * string) list
                | Combination of string * ((string * string) list);

(* remove: Filter -> tag -> bool

   Does the filter say that the tag should be removed?
 *)
fun remove (ByTagName tag) x = tag = tagName x
  | remove (ByAttributes []) x = false
  | remove (ByAttributes ((name, value) :: rest)) x =
        (case remove (ByAttributes rest) x of
            true => true
          | false => (case getAttribute name x of
                         NONE => false
                       | SOME v => value = v))
  | remove (Combination (tag, attrs)) x = remove (ByTagName tag) x
                                          orelse
                                          remove (ByAttributes attrs) x

(* Reverse the argument order of a function. *)
fun revArgs f x y = f y x;
    
(* Filter list -> parsetree list -> parsetree list *)
fun filterhtml _ [] = []
  | filterhtml filters ((x as (Text _)) :: xs) = x :: (filterhtml filters xs)
  | filterhtml filters ((Tag (tag,subtrees)) :: xs) =
        if List.exists ((revArgs remove) tag) filters
        then filterhtml filters xs
        else Tag (tag, filterhtml filters subtrees)
             :: (filterhtml filters xs)

val standardfilter = [ByTagName "script",
                      ByTagName "style"];

end
