structure TextAnalysisReporter :> TextAnalysisReporter =
struct
open Msp;
infix &&;
open TextAnalyser;

fun span1 class content = mark1a "SPAN"
                                 ("class=\"" ^ class ^ "\"")
                                 content;

fun div1 class content = mark1a "DIV"
                                ("class=\"" ^ class ^ "\"")
                                content;
    
fun reportResult (Lix x) = span1 "lix" ($(Real.toString x))
  | reportResult (FleshReadingEase x) = span1 "fleshrl" ($(Real.toString x))
  | reportResult (FleshKincaidGradeLevel x) = span1 "fkincaidgl" ($(Real.toString x));

fun reportResults results = div1 "result" (prmap (fn x => (reportResult x) && br) results);

fun reportSentenceElem (WordResult (text, correct)) = if correct
                                                     then $ (htmlencode text)
                                                     else span1 "spellerror"
                                                                ($ (htmlencode text))
  | reportSentenceElem (PunctuationResult text) = $ text;

fun findLix results = List.find (fn Lix x => true
                                  | _ => false)
                                results;

fun reportSentence (results, elemResults) = 
    let
        val lix = case findLix results of
                      SOME (Lix x) => x
                    | _ => 0.0;

        val style = if lix > 38.0
                    then "hardsentence"
                    else "easysentence";
    in
        span1 style (prmap reportSentenceElem elemResults)
    end;

fun reportSentences sentences = prmap reportSentence sentences;

fun reportContent (ParagraphResult (results, sentences, descriptions)) =
    let
        val resultReport = reportResults results;
        val sentencesReport = reportSentences sentences;
        val descriptionsReport = prmap reportSentences descriptions;
    in
        resultReport && (p (sentencesReport && descriptionsReport)) && hr
    end
  | reportContent (HeadingResult (result, content)) = 
    let
        val resultReport = reportResults result;
        val contentReport = prmap reportContent content;
    in
        resultReport && (h1 contentReport) && hr
    end
  | reportContent (QuotationResult (result, content)) = 
    let
        val resultReport = reportResults result;
        val contentReport = prmap reportContent content;
    in
        resultReport && (blockquote contentReport) && hr
    end;

val style = mark1 "STYLE"
                  ($ (".lix { background: green; }" ^
                      ".fleshrl { background: blue; }" ^
                      ".fkincaidgl { background: yellow; }" ^
                      "/*.result {float:right;}*/ " ^
                      ".hardsentence {background: pink;}" ^
                      ".easysentence {background: lightgreen;}"))
                                         

fun makeReport' ({title_results,
                  document_results,
                  content_results} : documentresult) =
    let
        val contentReport = (prmap reportContent content_results);
    in
        (html (head (style && (title ($ "results..")))
              &&
              body contentReport))
         handle Match => $ "her"
    end;

val makeReport = flatten o makeReport';

end;
