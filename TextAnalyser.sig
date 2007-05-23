signature TextAnalyser =
sig

type text = TextExtractor.text;

(* Result of a single word,
   the boolean indicates whether the word is spelled correctly or not. *)
type wordresult = text * bool

datatype SentenceElementResult = Word of wordresult
                               | Punctuation of text;

datatype AnalysisResult = Lix of real
                        | FleshReadingEase of real
                        | FleshKincaidGradeLevel of real;

(* Results of sentences *)
type sentenceresult = (SentenceElementResult list) * (AnalysisResult list);

(* Results from a paragraph, a heading or a quotation *)
datatype textresult = ParagraphResult of AnalysisResult list *
                                         sentenceresult list *
                                         sentenceresult list
                    | HeadingResult of AnalysisResult list *
                                       textresult list
                    | QuotationResult of AnalysisResult list *
                                         textresult list;
(* All results of a document *)
type documentresult = {title_results : sentenceresult,
                       document_results : (AnalysisResult list),
                       content_results : textresult list};
end
