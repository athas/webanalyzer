signature TextAnalysisReporter =
sig
    val colorByResults : TextAnalyser.results -> string;
    val makeReport : TextAnalyser.documentresult -> string;
end;
