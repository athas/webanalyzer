structure Help :> Help =
struct

fun printProgramArgs () = 
    let
        fun p str = print str before print "\n";
    in
        (p " --- Webanalyzer ---";
         p "";
         p "\"url\" [-d num] [-u str] [-c num] [-lix] [-fre] [-fkgl]";
         p "valgfrie argumenter er:";
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

         p "")
    end;

end
