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
                           | Bidirectional of TextDirection; (* Should the words be reversed? *)

    (* A text format where most of HTML's nesting is removed. *)
    datatype paragraphised = Paragraph of (text * WordAttribute list) list
                                          * text list
                           | Heading of paragraphised list
                           | Quotation of paragraphised list;

    type paragraphiseddocument = {title : text option,
                                  languagecode : text option,
                                  content : paragraphised list};

    val extractFromHTML : HTMLParser.parsetree list -> paragraphiseddocument;

    val addWordAttribute : WordAttribute -> WordAttribute list -> WordAttribute list

end
