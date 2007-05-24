structure TextAnalysisReporter :> TextAnalysisReporter =
struct
open Msp;
infix &&;
open TextAnalyser;


fun span1 class content = mark1a ("class=\"" ^ class ^ "\"")
                                 "SPAN"
                                 content;
    
fun reportResult (Lix x) = span1 "lix" ($(Real.toString x))
  | reportResult (FleshReadingEase x) = span1 "fleshrl" ($(Real.toString x))
  | reportResult (FleshKincaidGradeLevel x) = span1 "fkincaidgl" ($(Real.toString x));

fun reportResults results = span1 "result" (prmap reportResult results);

fun reportSentenceElem (WordResult (text, correct)) = if correct
                                                     then $ (htmlencode (text ^ " "))
                                                     else span1 "spellerror"
                                                                ($ (htmlencode text))
  | reportSentenceElem (PunctuationResult text) = $ (text ^ " ");

fun reportSentence (results, elemResults) = prmap reportSentenceElem elemResults;

fun reportSentences sentences = prmap reportSentence sentences;

fun reportContent (ParagraphResult (results, sentences, descriptions)) =
    let
        val resultReport = reportResults results;
        val sentencesReport = reportSentences sentences;
        val descriptionsReport = prmap reportSentences descriptions;
    in
        p (resultReport && (sentencesReport && descriptionsReport))
    end
  | reportContent (HeadingResult (result, content)) = 
    let
        val resultReport = reportResults result;
        val contentReport = prmap reportContent content;
    in
        h1 (contentReport && resultReport)
    end
  | reportContent (QuotationResult (result, content)) = 
    let
        val resultReport = reportResults result;
        val contentReport = prmap reportContent content;
    in
        blockquote (resultReport && contentReport)
    end;



fun makeReport' ({title_results,
                  document_results,
                  content_results} : documentresult) =
    let
        val contentReport = (prmap reportContent content_results);
    in
        (html (head (title ($ "results.."))
              &&
              body contentReport))
         handle Match => $ "her"
    end;

val makeReport = flatten o makeReport';

end;
