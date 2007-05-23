signature TextAnalyser =
sig

type text = TextExtractor.text;

(* Result of a single word,
   the boolean indicates whether the word is spelled correctly or not. *)
datatype SentenceElementResult = WordResult of text * bool
                               | PunctuationResult of text;

datatype AnalysisResult = Lix of real
                        | FleshReadingEase of real
                        | FleshKincaidGradeLevel of real;

type results = AnalysisResult list;

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
type documentresult = {title_results : results * sentenceresult list,
                       document_results : results,
                       content_results : textresult list};

val analyse : Sentencifier.document -> documentresult;
end
