signature TextExtractor =
sig
    type text;

    (* Attributes that a word can have *)
    datatype WordAttribute = Language of string
                           | Emphasized
                           | Code (* var, kbd *)
                           | Acronym of string option;
             
    type word = text * (WordAttribute list);
     
    (* Either a word, a space or punctuation *)
    datatype SentenceElement = Word of word
                             | Punctuation of text;
         
    (* [From Wikipedia] Punctuation is everything in written language
         other than the actual letters, including punctuation marks,
         inter-word spaces, capitalization, and indentation (Todd,
         2000). *)

    type sentence = SentenceElement list;

    (* The major elements of a text.
     
     A paragraph is a list of sentences and descriptive text used in
     the text (eg. image descriptions and footnotes). *)
    datatype textelement = Paragraph of sentence list * text list
                         | Heading of textelement list
                         | Quotation of textelement list
                     (*  | Code of text (* <code> *) *)

    type document = {title : sentence list option,
                     languagecode : string option,
                     content : textelement list};


    val extractText : HTMLParser.parsetree list -> document;

end
