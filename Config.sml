structure Config :> Config =
struct

val UserAgentDefault = "webanalyzer"
val DepthLimitDefault = 10;
val CrawlDelayDefault = 0;

(* Program User-agent info *)
val UserAgent = ref UserAgentDefault;
fun setHttpUserAgent str = UserAgent := str
fun httpUserAgent () = !UserAgent;

(* Crawl dept limit *)
val DepthLimit = ref DepthLimitDefault;
fun setCrawlDepthLimit newLimit = DepthLimit := newLimit;
fun crawlDepthLimit () = !DepthLimit;

(* Crawl delay indicationg how loong the site wants us to wait between
   each crawling links *)
val CrawlDelay = ref CrawlDelayDefault;
(* Used to set the crawlDelay, if it is specifyed by either the user
   or in a Robots.txt *) 
fun setCrawlDelay n = CrawlDelay := n;
(* Returns the crawlDelay de-refed *)
fun crawlDelay () = !CrawlDelay;

fun setDefaults () = 
    (setHttpUserAgent UserAgentDefault;
     setCrawlDepthLimit DepthLimitDefault;
     setCrawlDelay CrawlDelayDefault);

end
