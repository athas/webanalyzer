signature TextAnalysisReporter =
sig
    val makeReport : TextAnalyser.documentresult -> string;
end;
