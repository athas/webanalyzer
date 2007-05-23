structure Config :> Config =

struct

fun HttpUserAgent () = "webanalyzer";

val depthLimit = ref 10;

fun setCrawlDepthLimit newLimit = depthLimit := newLimit;
fun crawlDepthLimit () = !depthLimit;

end
