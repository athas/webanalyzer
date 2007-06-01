structure Help :> Help =
struct

fun lstConcat lst = 
    if List.length lst > 0 then
        List.foldl (fn (x,y) => if not (y = "") then (x ^ ", " ^ y) else x) "" lst
    else
        "*INGEN*";

fun printProgramArgs () = 
    let
        fun p str = print str before print "\n";
    in
        (p " --- Webanalyzer ---";
         p "";
         p "webanalyzer url [options]";
         p "";
         p "Options";
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
         p "\t -l lang ";
         p "\t\t Sætter default sprog kode som bruges hvis der ikke er angivet noget andet i det";
         p "\t\t analyserede dokument. 'lang' angives som ISO 639 sprog kode eventuelt efterfulgt";
         p "\t\t af en bindestreg ('-') eller underscore ('_') og 2 bogstavs ISO 3166 lande kode";
         p ("\t\t Default: " ^ Config.defaultLanguage());
         p "\t -o dir ";
         p "\t\t Sætter output mappen til en relativ eller absolut sti. Hvis der ikke er angivet";
         p "\t\t nogen sti bliver der oprettet en mappe (navngivet efter det domæne der er";
         p "\t\t angivet som url) i den mappe som programmet startes fra.";
         p ("\t\t Default: " ^ (if Config.outputDir() = NONE 
                              then OS.FileSys.getDir() 
                              else valOf (Config.outputDir())));
         p "\t -ignore-tag tag ";
         p "\t\t Angiver hvilket html tag der skal filtreres fra i analysen. Kan defineres";
         p "\t\t flere gang hvis flere tags ønskes filtreret fra.";
         p ("\t\t Default: " ^ lstConcat (Config.tagNameFilters()));
         p "\t -ignore-id id ";
         p "\t\t Angiver hvilket html tag med givet id der skal filtreres fra i analysen.";
         p "\t\t Kan defineres flere gang hvis flere tags ønskes filtreret fra.";
         p ("\t\t Default: " ^ lstConcat (Config.idFilters()));
         p "";
         p "Se også:";
         p "\t aspell(1)")
    end;

end
