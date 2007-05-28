signature Sentencifier =
sig

    type text = TextExtractor.text;

    (* Attributes that a word can have *)
    datatype WordAttribute = Language of text
                           | Emphasized
                           | Code (* var, kbd *)
                           | Acronym;


    type word = text * WordAttribute list;
     
    (* Either a word or punctuation.

       [From Wikipedia] Punctuation is everything in written language
       other than the actual letters, including punctuation marks,
       inter-word spaces, capitalization, and indentation (Todd,
       2000). *)
    datatype SentenceElement = Word of word
                             | Punctuation of text;

    (* A list of sentence elements, there should be space between each
    element. *)
    type sentence = SentenceElement list;

    datatype textelement = Paragraph of sentence list
                                        * sentence list list
                         | Heading of textelement list
                         | Quotation of textelement list;
                     (*  | Code of text (* <code> *) *)

    type document = {title : sentence list option,
                     languagecode : string option,
                     content : textelement list};

    (* Splits a TextExtractor.paragraphiseddocument in sentences and 
       words. If a WordAttribute is applied to any part of the text,
       that attribute is applied to all words in that particular part. *)
    val sentencify : TextExtractor.paragraphiseddocument
                         -> document;
end
