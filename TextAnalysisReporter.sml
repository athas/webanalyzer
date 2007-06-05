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

fun analyses () = 
      let
          val lix = if Config.lix ()
                    then SOME ("Lix", getLix, "http://da.wikipedia.org/wiki/Læsbarhedsindeks")
                    else NONE;
          val fre =  if Config.fre ()
                     then SOME ("Flesh Reading Ease", getFRE, "http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test#Flesch_Reading_Ease")
                     else NONE;
          val fkgl = if Config.fkgl ()
                     then SOME ("FK Grade Level", getFKGL, "http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test#Flesch.E2.80.93Kincaid_Grade_Level")
                     else NONE
      in
          Util.SOMEs [lix, fre, fkgl]
      end;

fun reportAnalyse results (title, analyse, link) =
              mark1 "SPAN" ((ahref link ($(title))) && ($(": " ^ (Util.formatForOutput
                                                  (analyse results)))) && br)

fun reportResults results = div1 "result" (prmap (reportAnalyse results) (analyses ()))

fun reportSentenceElem (WordResult (text, correct, repetition)) =
            if repetition
            then span1 "repetition" ($ (htmlencode text))
            else if correct
            then $ (htmlencode text)
            else span1 "spellerror" ($ (htmlencode text))
  | reportSentenceElem (PunctuationResult text) = $ (htmlencode text);

fun colorByResults results = 
    let
        val lowerlimit = 20.0
        val upperlimit = 80.0
        val multiplier = 100.0 / (upperlimit - lowerlimit)
        val lix = Real.max(lowerlimit, Real.min(getBadnessFactor results, upperlimit))
        val greenlevel = trunc (2.55 * (100.0 - (lix - lowerlimit) * multiplier))
        val redlevel = trunc (2.55 * ((lix - lowerlimit) * multiplier))
        fun hexify number = StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX number)
    in
        "#" ^ hexify(#r rgb) ^ hexify (#g rgb) ^ hexify (#b rgb)
    end;

fun createColorBox results content =
    let
        val color = colorByResults results;
    in
        mark1a "DIV"
               ("style=\"border-color:" ^ color ^ ";\" class=\"colorbox\"")
               content
    end

fun reportSentence color (results, elemResults) = 
        if color
        then 
            mark1a "SPAN"
                   ("style=\"background-color:" ^ (colorByResults results) ^ ";\"")
                   (prmap reportSentenceElem elemResults)
        else prmap reportSentenceElem elemResults

fun reportSentences color sentences = prmap (reportSentence color) sentences;

(* onlyContent indicates whether this is a complete paragraph or occurs in some other
   context where the only wanted analyze is the content-analyze. *)
fun reportContent onlyContent color (ParagraphResult (results, sentences, descriptions)) =
    let
        val resultReport = reportResults results;
        val sentencesReport = reportSentences color sentences;
        val descriptionsReport = prmap (li o (reportSentences color)) descriptions;
        val content = p (sentencesReport) && (if (length descriptions) > 0
                                              then p ((h4 ($ "Beskrivende tekst: "))
                                                      && (ul descriptionsReport))
                                              else Empty)
      
    in
        if onlyContent
        then content
        else createColorBox results (resultReport && content)
    end
  | reportContent display color (HeadingResult (result, content)) = 
    let
        val resultReport = reportResults result;
        val contentReport = prmap (reportContent true color) content;

    in
        if display 
        then h4 contentReport
        else
            createColorBox result
                           (resultReport && (h3 contentReport))
    end
  | reportContent display _ (QuotationResult content) = 
    let
        val contentReport = prmap (reportContent true false) content;
    in
        blockquote contentReport
    end;

val style = mark1 "STYLE"
                  ($ (".colorbox { border-width: 10px; border-style: solid; margin: 5px; padding: 10px; }"
                   (* ^ ".lix { background: lightgreen; }"
                      ^ ".fleshrl { background: lightblue; }"
                      ^ ".fkincaidgl { background: yellow; }" *)
                      ^ ".document {max-width: 750px;}"
                      ^ ".spellerror {border:2px solid blue;}"
                      ^ ".repetition {border:2px solid orange;}"
                   (* ^ ".result {margin-bottom: 10px; }" *)
                  ));
                                         

fun makeReport' ({titleResults,
                  documentResults,
                  contentResults} : documentresult) =
    let
        val contentReport = prmap (reportContent false true) contentResults;
        val titleReport = case titleResults of
                              NONE => $("No title found.")
                            | SOME (results, sentences) => createColorBox results ((reportResults results) && (reportSentences true sentences));
                                        
        val documentReport = reportResults documentResults;
    in
        (html (head (style && (title ($ "Results")))
              &&
              (body (div1 "document"
                          ((h1 ($ "Dokument resultat:")) && (createColorBox documentResults documentReport) &&
                          (h1 ($ "Titel resultat:")) && titleReport &&
                          (h1 ($ "Resultater for indhold:") &&  contentReport))))))
    end;

val makeReport = flatten o makeReport';

end;
