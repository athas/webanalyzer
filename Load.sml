load "Regex";
load "Socket";
load "Unix";

app use ["Util.sig", "Util.sml",
         "Config.sig", "Config.sml",
         "Http.sig", "Http.sml",
         "HTMLLexer.sig", "HTMLLexer.sml",
         "HTMLParser.sig", "HTMLParser.sml",
         "HTMLFilter.sig", "HTMLFilter.sml",
         "TextAnalyser.sig",
         "HTMLTextExtractor.sig", "HTMLTextExtractor.sml",
         "Robots.sig", "Robots.sml"];
