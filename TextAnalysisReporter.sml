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
    
fun reportResult (Lix x) = span1 "lix" ($("Lix: " ^ (Real.toString x)))
  | reportResult (FleshReadingEase x) = span1 "fleshrl" ($("Flesh Reading Ease: " ^ (Real.toString x)))
  | reportResult (FleshKincaidGradeLevel x) = span1 "fkincaidgl" ($("FK Grade level: " ^ (Real.toString x)));

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
        val lowerlimit = 20.0
        val upperlimit = 80.0
        val multiplier = 100.0 / (upperlimit - lowerlimit)
        val lix = Real.max(lowerlimit, Real.min(case findLix results of
                                            SOME (Lix x) => x
                                          | _ => 0.0,
                                          upperlimit))
        val greenlevel = trunc (2.55 * (100.0 - (lix - lowerlimit) * multiplier))
        val redlevel = trunc (2.55 * ((lix - lowerlimit) * multiplier))
        fun hexify number = StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX number)
        val color = "#" ^ hexify redlevel ^ hexify greenlevel ^ "00"
    in
        mark1a "SPAN" ("style=\"background-color:" ^ color ^ ";\"") (prmap reportSentenceElem elemResults)
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
        (h1 contentReport)
    end
  | reportContent (QuotationResult (result, content)) = 
    let
        val resultReport = reportResults result;
        val contentReport = prmap reportContent content;
    in
        (blockquote contentReport)
    end;

(*val style = mark1 "STYLE"
                  ($ (".lix { background: lightgreen; }" ^
                      ".fleshrl { background: lightblue; }" ^
                      ".fkincaidgl { background: yellow; }" ^
                      "/*.result {float:right;}*/ ")) *)
                                         

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
