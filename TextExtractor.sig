signature TextExtractor =
sig

    type text = string;

    datatype TextDirection = RightToLeft
                           | LeftToRight;

    (* Attributes that a word can have *)
    datatype WordAttribute = Language of text
                           | Emphasized
                           | Code (* var, kbd *)
                           | Acronym
                              (* Reverse word before spellchecking? *)
                           | Bidirectional of TextDirection;

    (* A text format where most of HTML's nesting is removed. *)
    datatype paragraphised = Paragraph of (text * WordAttribute list) list
                                          * text list
                           | Heading of paragraphised list
                           | Quotation of paragraphised list;

    (* A document partitioned in paragraphs *)
    type paragraphiseddocument = {title : text option,
                                  languagecode : text option,
                                  content : paragraphised list};

    (* Create a paragraphised document from a HTML-parsetree *)
    val extractFromHTML : HTMLParser.parsetree list
                          -> paragraphiseddocument;

    (* Add an attribute to a list of attributes. *)
    val addWordAttribute : WordAttribute
                           -> WordAttribute list
                           -> WordAttribute list;

end;
