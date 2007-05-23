load "Regex";
load "Socket";
load "Unix";
load "Binarymap";

app use ["Util.sig", "Util.sml",
         "Config.sig", "Config.sml",
         "Http.sig", "Http.sml",
         "HTMLLexer.sig", "HTMLLexer.sml",
         "HTMLParser.sig", "HTMLParser.sml",
         "HTMLFilter.sig", "HTMLFilter.sml",
         "TextExtractor.sig", "TextExtractor.sml",
         "Sentencifier.sig", "Sentencifier.sml",
         "TextAnalyser.sig",
         "Robots.sig", "Robots.sml"];
