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

    (* A text format where most of HTML's nesting is removed and
       paragraphs, headings and quotations is identified.  

       A paragraph consist of some text and a list of descriptions
       associated with different parts of that text. The text is
       divided in parts with similar attributes (same language etc.)
       Headings and Quotations is just considered as a list of paragraphs. *)
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
