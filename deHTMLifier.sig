signature deHTMLifier =
sig
    type text;

         
    datatype TextDirection = LeftToRight | RightToLeft;

    (* Attributes that a word can have *)
    datatype WordAttribute = Language of string
                           | Emphasized
                           | Code (* var, kbd *)
                           | Acronym of string option
                           | Bidirectional of TextDirection;

    type word = text * (WordAttribute Binaryset.set);
         
    (* Either a word, a space or punctuation *)
    datatype SentenceElement = Word of word
                             | Punctuation of text;
             
    (* [From Wikipedia] Punctuation is everything in written language
                        other than the actual letters, including
                        punctuation marks, inter-word spaces,
                        capitalization, and indentation (Todd,
                        2000). *)

type sentence = SentenceElement list;

datatype textelement = Paragraph of sentence list
                     | Heading of textelement list
                     | Quotation of textelement list
                     | Description of text; (* for alt, title and
                                              summary attributes. *)
                 (*  | Link of textelement list 
                     | Code of text (* <code> *) 
                                     *)

type document = {title : string option,
                 languagecode : string option,
                 content : textelement list};


    val dehtmlify : HTMLParser.parsetree list -> document;

end
