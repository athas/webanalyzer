structure Config :> Config =
struct

(* Program User-agent info *)
val UserAgent = ref "webanalyzer";
fun setHttpUserAgent str = UserAgent := str
fun httpUserAgent () = !UserAgent;

(* Crawl dept limit *)
val DepthLimit = ref 10;
fun setCrawlDepthLimit newLimit = DepthLimit := newLimit;
fun crawlDepthLimit () = !DepthLimit;

(* Crawl delay indicationg how loong the site wants us to wait between
   each crawling links *)
val CrawlDelay = ref 0;
(* Used to set the crawlDelay, if it is specifyed by either the user
   or in a Robots.txt *) 
fun setCrawlDelay n = CrawlDelay := n;
(* Returns the crawlDelay de-refed *)
fun crawlDelay () = !CrawlDelay;


fun setDefault () = 
    (
     setHttpUserAgent "webanalyzer";
     setCrawlDepthLimit 10;
     setCrawlDelay 0
    )
     

end
