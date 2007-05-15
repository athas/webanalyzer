structure HTMLLexer :> HTMLLexer =
struct

local
    (* (Beginning, chars since beginning) indices. *)
    type runningIndices = int * int;
    type tagNameIndices = runningIndices;
    type attributeNameIndices = runningIndices;
    type attributeValueIndices = runningIndices;
    type attributeIndices = attributeNameIndices * attributeValueIndices;

    datatype lexerstate = Done of (HTMLLexeme.lexeme * lexerstate)
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

    fun findAttributes string attributeList = map (fn ((ni, nc), (vi, vc)) =>
                                                      (substring(string, ni, nc),
                                                       substring(string, vi, vc)))
                                                  attributeList;

    fun makeStartTag string (ti, tc) attributeIndicesList =
        HTMLLexeme.startTag (substring(string, ti, tc), 
                             findAttributes string attributeIndicesList);

    fun makeEndTag string (ti, tc) attributeIndicesList =
        HTMLLexeme.endTag (substring(string, ti, tc), 
                           findAttributes string attributeIndicesList);

    fun lexer string (New i) #"<" =
        LexingTag (i + 1, 0)
      | lexer string (New i) _ =
        LexingText (i, 1)
      | lexer string (LexingText (i, c)) #"<" =
        Done (HTMLLexeme.Text (substring(string, i, c)), LexingTag (i + c + 1, 1))
      | lexer string (LexingText (i, c)) _ =
        LexingText (i, c + 1)
      | lexer string (LexingTag (i, c)) #"/" =
        LexingEndTag (i + 1, 1)
      | lexer string (LexingTag (i, c)) #"!" =
        LexingIgnorableTag ((i + 1), 1)
      | lexer string (LexingTag (i, c)) #"?" =
        LexingIgnorableTag ((i + 1), 1)
      | lexer string (LexingTag (i, c)) _ =
        LexingTagName (false, (i, 1))
      | lexer string (LexingEndTag (i, c)) _ =
        LexingTagName (true, (i, 1))
      | lexer string (LexingIgnorableTag(i, 1)) #"-" =
        LexingMaybeCommentTag (i + 1)
      | lexer string (LexingIgnorableTag (i, _)) #">" =
        New (i + 1)
      | lexer string (LexingIgnorableTag (i, c)) _ =
        LexingIgnorableTag ((i + 1), c)
      | lexer string (LexingMaybeCommentTag i) #"-" =
        LexingCommentTag (i + 1)
      | lexer string (LexingMaybeCommentTag i) _ =
        LexingIgnorableTag (i + 1, 2)
      | lexer string (LexingCommentTag i) #"-" =
        LexingMaybeEndComment (i + 1)
      | lexer string (LexingCommentTag i) _ =
        LexingCommentTag (i + 1)
      | lexer string (LexingMaybeEndComment i) #"-" =
        LexingEndComment (i + 1)
      | lexer string (LexingMaybeEndComment i) _ =
        LexingCommentTag (i + 1)
      | lexer string (LexingEndComment i) #">" =
        New (i + 1)
      | lexer string (LexingEndComment i) _ =
        LexingCommentTag (i + 1)
      | lexer string (LexingTagName (false, (i, c))) #">" =
        Done (makeStartTag string (i, c) [], New (i + c + 1))
      | lexer string (LexingTagName (true, (i, c))) #">" =
        Done (makeEndTag string (i, c) [], New (i + c + 1))
      | lexer string (LexingTagName (endTag, (i, c))) #" " =
        LexingAttributes (endTag, (i, c), [], i + c + 1)
      | lexer string (LexingTagName (endTag, (i, c))) _ =
        LexingTagName (endTag, (i, c + 1))
      | lexer string (LexingAttributes (endTag, tagNameIndices, attributeIndices, i)) #" " =
        LexingAttributes (endTag, tagNameIndices, attributeIndices, i + 1)
      | lexer string (LexingAttributes (false, tagNameIndices, attributeIndices, i)) #">" =
        Done (makeStartTag string tagNameIndices attributeIndices, New (i + 1))
      | lexer string (LexingAttributes (true, tagNameIndices, attributeIndices, i)) #">" =
        Done (makeEndTag string tagNameIndices attributeIndices, New (i + 1))
      | lexer string (LexingAttributes (endTag, tagNameIndices, attributeIndices, i)) _ =
        LexingAttributeName (endTag, tagNameIndices, attributeIndices, (i, 1))
      | lexer string (LexingAttributeName (endTag, tagNameIndices, attributeIndices, (i, c))) #"=" =
        LexingAttributeValueStart (endTag, tagNameIndices, attributeIndices, (i, c), i + c + 1)
      | lexer string (LexingAttributeName (endTag, tagNameIndices, attributeIndices, (i, c))) _ =
        LexingAttributeName (endTag, tagNameIndices, attributeIndices, (i, c + 1))
      | lexer string (LexingAttributeValueStart (endTag, tagNameIndices, attributeIndices, attributeNameIndices, i)) #"\"" =
        LexingAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i + 1, 0))
      | lexer string (LexingAttributeValueStart (endTag, tagNameIndices, attributeIndices, attributeNameIndices, i)) _ =
        LexingUndelimitedAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, 1))
      | lexer string (LexingAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) #"\"" =
        LexingAttributes (endTag, tagNameIndices, (attributeNameIndices, (i, c)) :: attributeIndices, i + c + 1)
      | lexer string (LexingAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) _ =
        LexingAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c + 1))
      | lexer string (LexingUndelimitedAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) #" " =
        LexingAttributes (endTag, tagNameIndices, (attributeNameIndices, (i, c)) :: attributeIndices, i + c + 1)
      | lexer string (LexingUndelimitedAttributeValue (false, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) #">" =
        Done (makeStartTag string tagNameIndices ((attributeNameIndices, (i, c)) :: attributeIndices), New (i + 1))
      | lexer string (LexingUndelimitedAttributeValue (true, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) #">" =
        Done (makeEndTag string tagNameIndices ((attributeNameIndices, (i, c)) :: attributeIndices), New (i + 1))
      | lexer string (LexingUndelimitedAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c))) _ =
        LexingUndelimitedAttributeValue (endTag, tagNameIndices, attributeIndices, attributeNameIndices, (i, c + 1))
      | lexer _ (Done _) _ = raise Fail "Lexer should not be passed Done state.";

    fun reader' string state getc cs lexemes = 
        case getc cs of
            SOME (c, cs) => (case lexer string state c of
                                 Done (lexeme, newstate) => reader' string newstate getc cs (lexeme::lexemes)
                               | Other => reader' string Other getc cs lexemes)
          | NONE => (lexemes, cs);

    fun reader string getc cs = SOME (reader' string (New 0) getc cs []);
in
fun lex string = case StringCvt.scanString (reader string) string of
                     SOME list => rev list
                   | NONE => raise Fail "Something went very wrong.";
end

end;
