structure TextAnalyser =
struct

type text = string;
         
(* Attributes that a word can have *)
datatype WordAttribute = Language of text
                       | Emphasized
                       | Code (* var, kbd *)
                       | Acronym;


type word = text * WordAttribute list;
     
(* Either a word, a space or punctuation *)
datatype SentenceElement = Word of word
                         | Punctuation of text;
         
(* [From Wikipedia] Punctuation is everything in written language
                    other than the actual letters, including
                    punctuation marks, inter-word spaces,
                    capitalization, and indentation (Todd, 2000). *)

type sentence = SentenceElement list;

datatype textelement = Paragraph of sentence list * text list
                     | Heading of textelement list
                     | Quotation of textelement list
                 (*  | Code of text (* <code> *) *)

type document = {title : sentence list option,
                 languagecode : string option,
                 content : textelement list};


end
