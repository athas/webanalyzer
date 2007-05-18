signature deHTMLifier =
sig
    type text;

           
    datatype TextDirection = LeftToRigt | RightToLeft;

    datatype WordAttribute = Language of string
                           | Emphasized
                           | Acronym of string option
                           | Bidirectional of TextDirection;

    type word = text * WordAttribute list;
         
    datatype SentenceElement = Word of word
                             | NonWord of text;
             
    type sentence = SentenceElement list
                    
    datatype textelement = Paragraph of sentence list
                         | Heading of textelement list
                         (*  | Table of *)
                         | Link of textelement list
                         | Quotation of textelement list
                         | Description of text (* for alt, title and summary attributes *)
                         | Code of text (* var, code, kbd *)
                               
    type document = {title : string option,
                     languagecode : string option,
                     content : textelement list};

    val dehtmlify : HTMLParser.parsetree list -> document;

end
