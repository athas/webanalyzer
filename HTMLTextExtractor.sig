signature HTMLTextExtractor =
sig

    val extractText : HTMLParser.parsetree list -> TextAnalyser.document;

end
