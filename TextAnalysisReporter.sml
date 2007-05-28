structure TextAnalysisReporter :> TextAnalysisReporter =
struct
open HTMLBuilder;
infix &&;
open TextAnalyser;

fun span1 class content = mark1a "SPAN"
                                 ("class=\"" ^ class ^ "\"")
                                 content

fun div1 class content = mark1a "DIV"
                                ("class=\"" ^ class ^ "\"")
                                content


fun reportResult (Lix x) = span1 "lix" ($("Lix: " ^ (Util.formatForOutput x)))
  | reportResult (FleshReadingEase x) =
        span1 "fleshrl" ($("Flesh Reading Ease: " ^ (Util.formatForOutput x)))
  | reportResult (FleshKincaidGradeLevel x) =
        span1 "fkincaidgl" ($("FK Grade level: " ^ (Util.formatForOutput x)))

fun reportResults results = div1 "result" (prmap (fn x => (reportResult x) && br) results)

fun reportSentenceElem (WordResult (text, correct)) = if correct
                                                     then $ (htmlencode text)
                                                     else span1 "spellerror"
                                                                ($ (htmlencode text))
  | reportSentenceElem (PunctuationResult text) = $ (htmlencode text);

fun findLix results = List.find (fn Lix x => true
                                  | _ => false)
                                results;

fun colorByResults results = 
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
    in
        "#" ^ hexify redlevel ^ hexify greenlevel ^ "00"
    end;

fun createColorBox results content =
    let
        val color = colorByResults results;
    in
        mark1a "DIV"
               ("style=\"border-color:" ^ color ^ ";\" class=\"colorbox\"")
               content
    end

fun reportSentence (results, elemResults) = 
    let
        val color = colorByResults results;
    in
        mark1a "SPAN"
               ("style=\"background-color:" ^ color ^ ";\"")
               (prmap reportSentenceElem elemResults)
    end;

fun reportSentences sentences = prmap reportSentence sentences;

(* onlyContent indicates whether this is a complete paragraph or occurs in some other
   context where the only wanted analyze is the content-analyze. *)
fun reportContent onlyContent (ParagraphResult (results, sentences, descriptions)) =
    let
        val resultReport = reportResults results;
        val sentencesReport = reportSentences sentences;
        val descriptionsReport = prmap (li o reportSentences) descriptions;
        val content = p (sentencesReport) && (if (length descriptions) > 0
                                              then p ((h4 ($ "Descriptions: "))
                                                      && (ul descriptionsReport))
                                              else Empty)
      
    in
        if onlyContent
        then content
        else createColorBox results ((p resultReport) && content)
    end
  | reportContent display (HeadingResult (result, content)) = 
    let
        val resultReport = reportResults result;
        val contentReport = prmap (reportContent true) content;

    in
        createColorBox result
                       (resultReport && (h3 contentReport))
    end
  | reportContent display (QuotationResult (result, content)) = 
    let
        val resultReport = reportResults result;
        val contentReport = prmap (reportContent true) content;
        val color = colorByResults result;
    in
        createColorBox result
                       (resultReport && (blockquote contentReport))
    end;

val style = mark1 "STYLE"
                  ($ (".colorbox { border-width: 10px; border-style: solid; margin: 5px; padding: 10px; }"
                   (* ^ ".lix { background: lightgreen; }"
                      ^ ".fleshrl { background: lightblue; }"
                      ^ ".fkincaidgl { background: yellow; }" *)
                      ^ ".document {max-width: 750px;}"
                      ^ ".spellerror {border:2px solid blue;}"
                   (* ^ ".result {margin-bottom: 10px; }" *)
                  ));
                                         

fun makeReport' ({title_results,
                  document_results,
                  content_results} : documentresult) =
    let
        val contentReport = prmap (reportContent false) content_results;
        val titleReport = case title_results of
                              NONE => $("No title found.")
                            | SOME (results, sentences) => createColorBox results ((reportResults results) && (reportSentences sentences));
                                        
        val documentReport = reportResults document_results;
    in
        (html (head (style && (title ($ "Results")))
              &&
              (body (div1 "document"
                          ((h1 ($ "Document results: ")) && (createColorBox document_results documentReport) &&
                          (h1 ($ "Page title results: ")) && titleReport &&
                          (h1 ($ "Content results:") &&  contentReport))))))
    end;

val makeReport = flatten o makeReport';

end;
