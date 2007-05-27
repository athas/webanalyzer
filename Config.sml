structure Config :> Config =
struct

val UserAgentDefault = "webanalyzer";
val DepthLimitDefault = 10;
val CrawlDelayDefault = 0;
val LixDefault = true;
val FreDefault = true;
val FkglDefault = true;
val SpellDefault = true;

(* Location of the scandinavian vowels in ISO 8859-1 *)
val scandinavianVowels = [chr 230, (* æ *)
                          chr 198, (* Æ *)
                          chr 248, (* ø *)
                          chr 216, (* Ø *)
                          chr 229, (* å *)
                          chr 197  (* Å *)
                         ]; 

val vowels = explode "aeiouyAEIOUY" @ scandinavianVowels;

fun isVowel char = Util.member(char, vowels); 
fun isAlphabetic char = Char.isAlpha char orelse Util.member(char, scandinavianVowels);

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

(* 'Flesch Reading Ease' analyze text. Set by commandline *)
val Fre = ref FreDefault;
fun setFre () = Fre := true;
fun fre () = !Fre;


(* 'Flesch-Kincaid Readability Test' analyze text. Set by commandline *)
val Fkgl = ref FkglDefault;
fun setFkgl () = Fkgl := true;
fun fkgl () = !Fkgl;

(* Spell check the text. Set by commandline *)
val Spell = ref SpellDefault;
fun setSpell () = Spell := true;
fun spell () = !Spell

fun setDefaults () = 
    (setHttpUserAgent UserAgentDefault;
     setCrawlDepthLimit DepthLimitDefault;
     setCrawlDelay CrawlDelayDefault;
     Lix := LixDefault;
     Fkgl := FkglDefault;
     Fre := FreDefault;
     Spell := SpellDefault);

end
