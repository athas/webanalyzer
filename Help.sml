structure Help :> Help =
struct

fun printProgramArgs () = 
    let
        fun p str = print str before print "\n";
    in
        (p " --- Webanalyzer ---";
         p "";
         p "webanalyzer \"url\" [-d num] [-u str] [-c num] [-lix] [-fre] [-fkgl]";
         p "";
         p "\t -d num ";
         p "\t\t Overskriver dybden af links som crawleren skal følge.";
         p ("\t\t Default: " ^ (Int.toString (Config.crawlDepthLimit ())));
         p "\t -u str ";
         p "\t\t Overskriver den User-agent som sendes med alle HTTP requests";
         p "\t\t og som der identifiseres med i robots.txt filer";
         p ("\t\t Default: " ^ Config.httpUserAgent ());
         p "\t -c num ";
         p "\t\t Overskriver pausen mellem hver HTTP request.";
         p ("\t\t Default: " ^ Int.toString (Config.crawlDelay ()));
         p "\t -lix ";
         p "\t\t Toggler hvorvidt der skal bruges lix analyse på teksten.";
         p "\t\t Læs mere: http://da.wikipedia.org/wiki/LIX";
         p ("\t\t Default: " ^ Bool.toString (Config.lix()));
         p "\t -fre ";
         p "\t\t Toggler hvorvidt der skal bruges 'Flesch Reading Ease' analyse på teksten.";
         p "\t\t Læs mere: http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test";
         p ("\t\t Default: " ^ Bool.toString (Config.fre()));
         p "\t -fkgl ";
         p "\t\t Toggler hvorvidt der skal bruges 'Flesch-Kincaid Grade Level' analyse på teksten.";
         p "\t\t Læs mere: http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test";
         p ("\t\t Default: " ^ Bool.toString (Config.fkgl()));
                                            
         p "")
    end;

end
