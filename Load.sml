(* Note that you have to load Main.sml seperately. 

load "Regex";
load "Socket";
load "Unix";
load "Binarymap";
load "Msp";
load "Listsort"; *)

CM.autoload "smlnj-lib/RegExp/regexp-lib.cm";
CM.autoload "smlnj-lib/INet/inet-lib.cm";

structure RegexMatcher = RegExpFn (structure P=AwkSyntax; structure E=BackTrackEngine);

app use ["Util.sig", "Util.sml",
         "Config.sig", "Config.sml",
         "Help.sig", "Help.sml",
         "Http.sig", "Http.sml",
         "HTMLLexer.sig", "HTMLLexer.sml",
         "HTMLParser.sig", "HTMLParser.sml",
         "HTMLFilter.sig", "HTMLFilter.sml",
         "SpellChecker.sig", "SpellChecker.sml",
         "TextExtractor.sig", "TextExtractor.sml",
         "Sentencifier.sig", "Sentencifier.sml",
         "TextAnalyser.sig", "TextAnalyser.sml"(*,
         "TextAnalysisReporter.sig", "TextAnalysisReporter.sml",
         "Robots.sig", "Robots.sml"*)
];
