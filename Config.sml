structure Config :> Config =
struct

val UserAgentDefault = "webanalyzer";
val DepthLimitDefault = 10;
val CrawlDelayDefault = 0;
val LixDefault = false;
val FkrtDefault = false;
val FreDefault = false;
val SpellDefault = false;

(* Program User-agent info *)
val UserAgent = ref UserAgentDefault;
fun setHttpUserAgent str = UserAgent := str
fun httpUserAgent () = !UserAgent;

(* Crawl dept limit *)
val DepthLimit = ref DepthLimitDefault;
fun setCrawlDepthLimit newLimit = DepthLimit := newLimit;
fun crawlDepthLimit () = !DepthLimit;

(* Crawl delay indicationg how loong the site wants us to wait between
   each crawling links Set by commandline or robots.txt*)
val CrawlDelay = ref CrawlDelayDefault;
fun setCrawlDelay n = CrawlDelay := n;
fun crawlDelay () = !CrawlDelay;

(* 'Lix' analyze text, set by commandline *)
val Lix = ref LixDefault;
fun setLix () = Lix := true;
fun lix () = !Lix;

(* 'Flesch-Kincaid Readability Test' analyze text. Set by commandline *)
val Fkrt = ref FkrtDefault;
fun setFkrt () = Fkrt := true;
fun fkrt () = !Fkrt;

(* 'Flesch Reading Ease' analyze text. Set by commandline *)
val Fre = ref FreDefault;
fun setFre () = Fre := true;
fun fre () = !Fre;

(* Spell check the text. Set by commandline *)
val Spell = ref SpellDefault;
fun setSpell () = Spell := true;
fun spell () = !Spell

fun setDefaults () = 
    (setHttpUserAgent UserAgentDefault;
     setCrawlDepthLimit DepthLimitDefault;
     setCrawlDelay CrawlDelayDefault;
     Lix := LixDefault;
     Fkrt := FkrtDefault;
     Fre := FreDefault;
     Spell := SpellDefault);

end
