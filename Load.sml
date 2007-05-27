(* Note that you have to load Main.sml seperately. *)

CM.make "$/regexp-lib.cm";
CM.make "$/inet-lib.cm";

structure RegexMatcher = RegExpFn (structure P=AwkSyntax; structure E=BackTrackEngine);

app use ["Util.sig", "Util.sml",
         "Config.sig", "Config.sml",
         "HTMLBuilder.sig", "HTMLBuilder.sml",
         "Help.sig", "Help.sml",
         "Http.sig", "Http.sml",
         "HTMLLexer.sig", "HTMLLexer.sml",
         "HTMLParser.sig", "HTMLParser.sml",
         "HTMLFilter.sig", "HTMLFilter.sml",
         "SpellChecker.sig", "SpellChecker.sml",
         "TextExtractor.sig", "TextExtractor.sml",
         "Sentencifier.sig", "Sentencifier.sml",
         "TextAnalyser.sig", "TextAnalyser.sml",
         "TextAnalysisReporter.sig", "TextAnalysisReporter.sml",
         "Robots.sig", "DummyRobots.sml"
];
