signature TextAnalyser =
sig

type text = TextExtractor.text;

(* Result of a single word,
   the boolean indicates whether the word is spelled correctly or not. *)
datatype SentenceElementResult = WordResult of text * bool
                               | PunctuationResult of text;

type results;

(* Results of sentences *)
type sentenceresult = results * (SentenceElementResult list);

(* Results from a paragraph, a heading or a quotation *)
datatype textresult = ParagraphResult of results *
                                         sentenceresult list *
                                         sentenceresult list list
                    | HeadingResult of results *
                                       textresult list
                    | QuotationResult of results *
                                         textresult list;
(* All results of a document *)
type documentresult = {titleResults : (results * sentenceresult list) option,
                       documentResults : results,
                       contentResults : textresult list};

val getLix  : results -> real;
val getFRE  : results -> real;
val getFKGL : results -> real;

val titleResults : documentresult -> (results * sentenceresult list) option;
val documentResults : documentresult -> results;
val contentResults : documentresult -> textresult list;

val analyse : Sentencifier.document -> documentresult;
end
