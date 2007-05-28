structure Config :> Config =
struct

val UserAgentDefault = "webanalyzer";
val DepthLimitDefault = 10;
val CrawlDelayDefault = 0;
val DefaultLanguageDefault = "da";
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
(* Number of digits after decimal point in the output. *)
val fractionalDigits = 2;
val vowels = explode "aeiouyAEIOUY" @ scandinavianVowels;

infix 0 member; (* From Util *)
fun x member y = List.exists (fn z => z=x) y;

fun isVowel char = char member vowels; 
fun isAlphabetic char = Char.isAlpha char orelse
                        char member scandinavianVowels;

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

val DefaultLanguage = ref DefaultLanguageDefault;
fun setDefaultLanguage language = DefaultLanguage := language;
fun defaultLanguage _ = !DefaultLanguage;

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
     setDefaultLanguage DefaultLanguageDefault;
     Lix := LixDefault;
     Fkgl := FkglDefault;
     Fre := FreDefault;
     Spell := SpellDefault);

end
