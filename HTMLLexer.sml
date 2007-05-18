structure HTMLLexer :> HTMLLexer =
struct

open Util;

local 
    (* Known HTML entities and the characters they map to. *)
    val HTMLentities = [("lt", #"<"),
                        ("gt", #">"),
                        ("amp", #"&")];

    (* Find a replacement for the provided entity name. If the entity
    nam is not known, the full identity (including ampersand and
    semicolon) will be returned. *)
    fun entityReplacement entity = case assoc entity HTMLentities of
                                       SOME replacement => [replacement]
                                     | NONE => #"&" :: explode entity @ [#";"]

    (* Iterate through a list of characters, looking for HTML
    entities, and return a list of characters with the HTML entities
    replaced by the character they map to. *)
    fun convertEntities' (#"&" :: rest) [] acc = convertEntities' rest [#"&"] acc
      | convertEntities' (char :: rest) [] acc = convertEntities' rest [] (acc @ [char])
      | convertEntities' (#";" :: rest) (#"&" :: entrest) acc = 
        convertEntities' rest [] (acc @ entityReplacement (implode entrest))
      | convertEntities' (char :: rest) (#"&" :: entrest) acc = 
        if Char.isAlpha char then
            convertEntities' rest (#"&" :: entrest @ [char]) acc
        else 
            convertEntities' rest [] (acc @ (#"&" :: entrest) @ [char])
      | convertEntities' [] entacc acc = entacc @ acc
      | convertEntities' input entacc acc = raise Fail "Impossible situation.";
in
(* Iterate through a string, looking for HTML entities, and return
astring with the HTML entities replaced by the character they map
to. *)
fun convertEntities string = implode (convertEntities' (explode string) [] []);
end

(* A function for extracting a string from a substring. *)
val properString = implode o map Char.toLower o explode o Substring.string;

(* Use substrings for representing text. *)
type tag = substring * (substring * substring) list;
fun tagName ((name, _) : tag) = properString name;
fun getAttribute name (_, attributes) = 
    case List.find (fn (attrname, value) => name = properString attrname) attributes of
        SOME (_, value) => SOME (convertEntities (Substring.string value))
      | NONE => NONE;
fun mapAttributes f (_, attributes)= 
    map (fn (name, value) => f (properString name,
                                convertEntities (Substring.string value)))
        attributes;

type text = Substring.substring;
val textContents = convertEntities o Substring.string;

datatype lexeme = StartTagLexeme of tag
                | EndTagLexeme of tag
                | TextLexeme of text;

(* (Beginning, chars since beginning) indices, defined as types for
readability. *)
type runningIndices = int * int;
type tagNameIndices = runningIndices;
type attributeNameIndices = runningIndices;
type attributeValueIndices = runningIndices;
type attributeIndices = attributeNameIndices * attributeValueIndices;

(* Datatype used by the lexer function to keep track of its
progress. *)
datatype lexerstate = Done of (lexeme * lexerstate)
                    | LexingText of runningIndices
                    | LexingTag of runningIndices
                    | LexingEndTag of runningIndices
                    | LexingIgnorableTag of runningIndices
                    | LexingMaybeCommentTag of int
                    | LexingCommentTag of int
                    | LexingMaybeEndComment of int
                    | LexingEndComment of int
                    | LexingTagName of bool * tagNameIndices
                    | LexingAttributes of bool * tagNameIndices * attributeIndices list * int
                    | LexingAttributeName of bool * tagNameIndices * attributeIndices list * attributeNameIndices
                    | LexingAttributeValueStart of bool * tagNameIndices * attributeIndices list * attributeNameIndices * int
                    | LexingAttributeValue of bool * tagNameIndices * attributeIndices list * attributeNameIndices * attributeValueIndices
                    | LexingUndelimitedAttributeValue of bool * tagNameIndices * attributeIndices list * attributeNameIndices * attributeValueIndices
                    | New of int;
local

(* Given a string and a list of tuples containing ((beginning1,size1),
(beginning1,size2))-indices into the string, create a list of tuples
containing strings extracted from string using the indices. *)
fun findAttributes string attributeList = map (fn ((ni, nc), (vi, vc)) =>
                                                  (Substring.substring(string, ni, nc),
                                                   Substring.substring(string, vi, vc)))
                                              attributeList;

(* Given a string, an (index, size)-tuple, and a list of attribute
indices (see findAttributes), create a StartTagLexeme with information
extracted from the string. *)
fun makeStartTag string (ti, tc) attributeIndicesList =
    StartTagLexeme (Substring.substring (string, ti, tc),
                    findAttributes string attributeIndicesList);

(* Given a string, an (index, size)-tuple, and a list of attribute
indices (see findAttributes), create an EndTagLexeme with information
extracted from the string. *)
fun makeEndTag string (ti, tc) attributeIndicesList =
    EndTagLexeme (Substring.substring (string, ti, tc),
                  findAttributes string attributeIndicesList);

(* Given a string and an (index, size)-tuple, create a TextLexeme with
information extracted from the string. *)
fun makeText string (ti, tc) =
    TextLexeme (Substring.substring (string, ti, tc));

(* Lex a sequence of characters into a lexeme. Returns lexer states
that should be used for a subsequent call to the function. The Done
state is returned when a full lexeme has been lexed. 

1. argument: a function for creating start tag lexemes.

2. argument: a function for creating end tag lexemes.

3. argument: a function for creating text lexemes.

4. argument: the lexer state.

5. argument: a character. *)
fun lexer _ _ _ (New i) #"<" =
    LexingTag (i + 1, 0)
  | lexer _ _ _ (New i) _ =
    LexingText (i, 1)
  | lexer _ _ makeText (LexingText (i, c)) #"<" =
    Done (makeText (i, c), LexingTag (i + c + 1, 1))
  | lexer _ _ _ (LexingText (i, c)) _ =
    LexingText (i, c + 1)
  | lexer _ _ _ (LexingTag (i, c)) #"/" =
    LexingEndTag (i + 1, 1)
  | lexer _ _ _ (LexingTag (i, c)) #"!" =
    LexingIgnorableTag ((i + 1), 1)
  | lexer _ _ _ (LexingTag (i, c)) #"?" =
    LexingIgnorableTag ((i + 1), 1)
  | lexer _ _ _ (LexingTag (i, c)) _ =
    LexingTagName (false, (i, 1))
  | lexer _ _ _ (LexingEndTag (i, c)) _ =
    LexingTagName (true, (i, 1))
  | lexer _ _ _ (LexingIgnorableTag(i, 1)) #"-" =
    LexingMaybeCommentTag (i + 1)
  | lexer _ _ _ (LexingIgnorableTag (i, _)) #">" =
    New (i + 1)
  | lexer _ _ _ (LexingIgnorableTag (i, c)) _ =
    LexingIgnorableTag ((i + 1), c)
  | lexer _ _ _ (LexingMaybeCommentTag i) #"-" =
    LexingCommentTag (i + 1)
  | lexer _ _ _ (LexingMaybeCommentTag i) _ =
    LexingIgnorableTag (i + 1, 2)
  | lexer _ _ _ (LexingCommentTag i) #"-" =
    LexingMaybeEndComment (i + 1)
  | lexer _ _ _ (LexingCommentTag i) _ =
    LexingCommentTag (i + 1)
  | lexer _ _ _ (LexingMaybeEndComment i) #"-" =
    LexingEndComment (i + 1)
  | lexer _ _ _ (LexingMaybeEndComment i) _ =
    LexingCommentTag (i + 1)
  | lexer _ _ _ (LexingEndComment i) #">" =
    New (i + 1)
  | lexer _ _ _ (LexingEndComment i) _ =
    LexingCommentTag (i + 1)
  | lexer makeStartTag _ _ (LexingTagName (false, (i, c))) #">" =
    Done (makeStartTag (i, c) [], New (i + c + 1))
  | lexer _ makeEndTag _ (LexingTagName (true, (i, c))) #">" =
    Done (makeEndTag (i, c) [], New (i + c + 1))
  | lexer _ _ _ (LexingTagName (endTag, (i, c))) char =
    if Char.isSpace char then LexingAttributes (endTag, (i, c), [], i + c + 1)
    else LexingTagName (endTag, (i, c + 1))
  | lexer _ _ _ (LexingAttributes (endTag, tagNameIndices, attributeIndices, i)) #" " =
    LexingAttributes (endTag, tagNameIndices, attributeIndices, i + 1)
  | lexer _ _ _ (LexingAttributes (endTag, tagNameIndices, attributeIndices, i)) #"/" =
    LexingAttributes (endTag, tagNameIndices, attributeIndices, i + 1)
  | lexer makeStartTag _ _ (LexingAttributes (false, tagNameIndices, attributeIndices, i)) #">" =
    Done (makeStartTag tagNameIndices attributeIndices, New (i + 1))
  | lexer _ makeEndTag _ (LexingAttributes (true, tagNameIndices, attributeIndices, i)) #">" =
    Done (makeEndTag tagNameIndices attributeIndices, New (i + 1))
  | lexer _ _ _ (LexingAttributes (endTag, tagNameIndices, attributeIndices, i)) _ =
    LexingAttributeName (endTag, tagNameIndices, attributeIndices, (i, 1))
  | lexer _ _ _ (LexingAttributeName (endTag, tagNameIndices, attributeIndices, (i, c))) #"=" =
    LexingAttributeValueStart (endTag, tagNameIndices, attributeIndices, (i, c), i + c + 1)
  | lexer _ _ _ (LexingAttributeName (endTag, tagNameIndices, attributeIndices, (i, c))) _ =
    LexingAttributeName (endTag, tagNameIndices, attributeIndices, (i, c + 1))
  | lexer _ _ _ (LexingAttributeValueStart (endTag, tagNameIndices, attributeIndices, attributeNameIndices, i)) #"\"" =
    LexingAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i + 1, 0))
  | lexer _ _ _ (LexingAttributeValueStart (endTag, tagNameIndices, attributeIndices, attributeNameIndices, i)) _ =
    LexingUndelimitedAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, 1))
  | lexer _ _ _ (LexingAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) #"\"" =
    LexingAttributes (endTag, tagNameIndices, (attributeNameIndices, (i, c)) :: attributeIndices, i + c + 1)
  | lexer _ _ _ (LexingAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) _ =
    LexingAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c + 1))
  | lexer _ _ _ (LexingUndelimitedAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) #" " =
    LexingAttributes (endTag, tagNameIndices, (attributeNameIndices, (i, c)) :: attributeIndices, i + c + 1)
  | lexer makeStartTag _ _ (LexingUndelimitedAttributeValue (false, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) #">" =
    Done (makeStartTag tagNameIndices ((attributeNameIndices, (i, c)) :: attributeIndices), New (i + c + 1))
  | lexer _ makeEndTag _ (LexingUndelimitedAttributeValue (true, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) #">" =
    Done (makeEndTag tagNameIndices ((attributeNameIndices, (i, c)) :: attributeIndices), New (i + c + 1))
  | lexer _ _ _ (LexingUndelimitedAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) _ =
    LexingUndelimitedAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c + 1))
  | lexer _ _ _ (Done _) _ = raise Fail "Lexer should not be passed Done state.";

(* Given a lexer function, a lexer state, a character getter, a
character source and a list of lexemes lexed so far, return a list of
the lexemes that can be lexed from the characters extracted from the
character source. *)
fun reader' lexer state getc cs lexemes = 
    case getc cs of
        SOME (c, cs) => (case lexer state c of
                             Done (lexeme, newstate) => reader' lexer newstate getc cs (lexeme::lexemes)
                           | Other => reader' lexer Other getc cs lexemes)
      | NONE => (lexemes, cs);

(* Given a string, a character getter and a character source, return a
list of lexemes that can be lexed from characters extracted by the
character source. *)
fun reader string getc cs = SOME (reader' (lexer (makeStartTag string)
                                                 (makeEndTag string)
                                                 (makeText string))
                                          (New 0) getc cs []);
in
fun lex string = case StringCvt.scanString (reader string) string of
                     SOME list => rev list
                   | NONE => raise Fail "Something went very wrong.";
end

end;
