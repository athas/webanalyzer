
local 

(* Word Count 

   w = words
   p = periods / sentences
   lw  = long words >6
   v = vovels *)
type wordCount= {w : int, p : int, lw : int, v :  int}; 

(* Int to Real *)
val i2r = Real.fromInt

(* Sometimes we encounter a number that internally is stores wierd and
   by that wont give EQUAL when compared to it self, like
   '58.1764705882...' (the number calculated by txt4/res4) who yields
   LESS. So we use string comparison instead (Not as acurate but good
   enoughf for this

   fun realCompare(x, y) = Real.compare(x, y) = EQUAL; *) 
fun realCompare(x, y) = Real.toString x = Real.toString y;

fun calcLix ({w, p, lw, ...}: wordCount) = (i2r w / i2r p) + ((i2r lw * i2r 100) / i2r w);
fun calcFre ({w, p, v, ...} : wordCount) = 206.835 - 1.015*(i2r w / i2r p) - 84.6*(i2r v / i2r w);
fun calcFkgl ({w, p, v, ...} : wordCount) = 0.39*(i2r w / i2r p) + 11.8*(i2r v / i2r w) - 15.59;

fun getLix ({document_results, ...}: TextAnalyser.documentresult) =
    List.hd (List.mapPartial 
                 (fn x => case x of TextAnalyser.Lix y => SOME y | _ => NONE) 
                 document_results);

fun getFre ({document_results, ...}: TextAnalyser.documentresult) =
    List.hd (List.mapPartial 
                 (fn x => case x of TextAnalyser.FleshReadingEase y => SOME y | _ => NONE) 
                 document_results);

fun getFkgl ({document_results, ...}: TextAnalyser.documentresult) =
    List.hd (List.mapPartial 
                 (fn x => case x of TextAnalyser.FleshKincaidGradeLevel y => SOME y | _ => NONE) 
                 document_results);

val analyze = TextAnalyser.analyse o 
              Sentencifier.sentencify o 
              TextExtractor.extractFromHTML o
              HTMLParser.parse;

val beginHTML = "<html><body>";
val endHTML = "</body></html>";


(* Manually countet results *)
val txt1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, set eiusmod tempor incidunt et labore et dolore magna aliquam. " 
val res1 = {w=18, p=1, lw=5, v=43};


val txt2 = "Ut enim ad minim veniam, quis nostrud exerc. "
val res2 =  {w=8, p=1, lw=1, v=15};

val txt3 = "Irure dolor in reprehend incididunt ut labore et dolore magna aliqua. "
val res3 = {w=11, p=1, lw=2, v=27};

val txt4 = "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. "
val res4 = {w=17, p=1, lw=7, v=42};

(* Automatic concationation plus sumation of the individual txt results *)
val txt5 = txt1 ^ txt3;
val res5 = {w=((#w res1) + (#w res3)), 
            p=((#p res1) + (#p res3)), 
            lw=((#lw res1) + (#lw res3)), 
            v=((#v res1) + (#v res3))};

val txt6 = txt2 ^ txt4;
val res6 = {w=((#w res2) + (#w res4)), 
            p=((#p res2) + (#p res4)), 
            lw=((#lw res2) + (#lw res4)), 
            v=((#v res2) + (#v res4))};

val txt7 = txt5 ^ txt6;
val res7 = {w=((#w res5) + (#w res6)), 
            p=((#p res5) + (#p res6)), 
            lw=((#lw res5) + (#lw res6)), 
            v=((#v res5) + (#v res6))};

val analyzeResultTxt1 = analyze (beginHTML ^ txt1 ^  endHTML);
val analyzeResultTxt2 = analyze (beginHTML ^ txt2 ^  endHTML);
val analyzeResultTxt3 = analyze (beginHTML ^ txt3 ^  endHTML);
val analyzeResultTxt4 = analyze (beginHTML ^ txt4 ^  endHTML);
val analyzeResultTxt5 = analyze (beginHTML ^ txt5 ^  endHTML);
val analyzeResultTxt6 = analyze (beginHTML ^ txt6 ^  endHTML);
val analyzeResultTxt7 = analyze (beginHTML ^ txt7 ^  endHTML);

in

val testTextAnalyzerTxt1Lix = realCompare(getLix analyzeResultTxt1, calcLix res1);
val testTextAnalyzerTxt2Lix = realCompare(getLix analyzeResultTxt2, calcLix res2);
val testTextAnalyzerTxt3Lix = realCompare(getLix analyzeResultTxt3, calcLix res3);
val testTextAnalyzerTxt4Lix = realCompare(getLix analyzeResultTxt4, calcLix res4);
val testTextAnalyzerTxt5Lix = realCompare(getLix analyzeResultTxt5, calcLix res5);
val testTextAnalyzerTxt6Lix = realCompare(getLix analyzeResultTxt6, calcLix res6);
val testTextAnalyzerTxt7Lix = realCompare(getLix analyzeResultTxt7, calcLix res7);

val testTextAnalyzerTxt1Fre = realCompare(getFre analyzeResultTxt1, calcFre res1);
val testTextAnalyzerTxt2Fre = realCompare(getFre analyzeResultTxt2, calcFre res2);
val testTextAnalyzerTxt3Fre = realCompare(getFre analyzeResultTxt3, calcFre res3);
val testTextAnalyzerTxt4Fre = realCompare(getFre analyzeResultTxt4, calcFre res4);
val testTextAnalyzerTxt5Fre = realCompare(getFre analyzeResultTxt5, calcFre res5);
val testTextAnalyzerTxt6Fre = realCompare(getFre analyzeResultTxt6, calcFre res6);
val testTextAnalyzerTxt7Fre = realCompare(getFre analyzeResultTxt7, calcFre res7);

val testTextAnalyzerTxt1Fkgl = realCompare(getFkgl analyzeResultTxt1, calcFkgl res1);
val testTextAnalyzerTxt2Fkgl = realCompare(getFkgl analyzeResultTxt2, calcFkgl res2);
val testTextAnalyzerTxt3Fkgl = realCompare(getFkgl analyzeResultTxt3, calcFkgl res3);
val testTextAnalyzerTxt4Fkgl = realCompare(getFkgl analyzeResultTxt4, calcFkgl res4);
val testTextAnalyzerTxt5Fkgl = realCompare(getFkgl analyzeResultTxt5, calcFkgl res5);
val testTextAnalyzerTxt6Fkgl = realCompare(getFkgl analyzeResultTxt6, calcFkgl res6);
val testTextAnalyzerTxt7Fkgl = realCompare(getFkgl analyzeResultTxt7, calcFkgl res7);

end
