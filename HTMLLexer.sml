structure HTMLLexer :> HTMLLexer =
struct

open HTMLLexeme;

local
    (* (Beginning, chars since beginning) indices. *)
    type runningIndices = int * int;
    type tagNameIndices = runningIndices;
    type attributeNameIndices = runningIndices;
    type attributeValueIndices = runningIndices;
    type attributeIndices = attributeNameIndices * attributeValueIndices;

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
        val HTMLentities = [("lt", #"<"),
                            ("gt", #">"),
                            ("amp", #"&")];

        fun assoc ((potentialKey, value) :: rest) key = if potentialKey = key then SOME(value)
                                                        else assoc rest key
          | assoc [] key = NONE

        fun entityReplacement entity = case assoc HTMLentities entity of
                                           SOME replacement => [replacement]
                                         | NONE => #"&" :: explode entity @ [#";"]
                                                   
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
    fun convertEntities string = implode (convertEntities' (explode string) [] []);
    end

    fun findAttributes string attributeList = map (fn ((ni, nc), (vi, vc)) =>
                                                      (substring(string, ni, nc),
                                                       substring(string, vi, vc)))
                                                  attributeList;

    fun makeStartTag string (ti, tc) attributeIndicesList =
        StartTagLexeme (convertEntities (substring (string, ti, tc)),
                             findAttributes string attributeIndicesList);

    fun makeEndTag string (ti, tc) attributeIndicesList =
        EndTagLexeme (convertEntities (substring (string, ti, tc)),
                           findAttributes string attributeIndicesList);

    fun makeText string (ti, tc) =
        TextLexeme (convertEntities (substring (string, ti, tc)));

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
      | lexer _ _ _ (LexingTagName (endTag, (i, c))) #" " =
        LexingAttributes (endTag, (i, c), [], i + c + 1)
      | lexer _ _ _ (LexingTagName (endTag, (i, c))) _ =
        LexingTagName (endTag, (i, c + 1))
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

    fun reader' lexer state getc cs lexemes = 
        case getc cs of
            SOME (c, cs) => (case lexer state c of
                                 Done (lexeme, newstate) => reader' lexer newstate getc cs (lexeme::lexemes)
                               | Other => reader' lexer Other getc cs lexemes)
          | NONE => (lexemes, cs);

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
