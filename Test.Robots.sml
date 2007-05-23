
(* Initialize Robots test *)
Robots.initRobotsTxt (Util.readFrom "robots.txt");


(* Test that we aren't disallowed from places that other User-agents
   are *)


(* Paths that is disallowed for User-agent: 'foo' *)
val testIsPathDisallowed001 = Robots.isPathAllowed "/sprint_xhtml" = true;
val testIsPathDisallowed002 = Robots.isPathAllowed "/pqa" = true;


(* Paths that is disallowed for User-agent: 'bar' *)
val testIsPathDisallowed011 = Robots.isPathAllowed "/videoprograminfo?" = true;
val testIsPathDisallowed012 = Robots.isPathAllowed "/sms/demo?" = true;
val testIsPathDisallowed013 = Robots.isPathAllowed "/katrina?" = true;


(* Test that we can handle directories ending with / or not *)
val testIsPathDisallowed021 = Robots.isPathAllowed "/archivesearch/advanced_search/" = false;
val testIsPathDisallowed022 = Robots.isPathAllowed "/archivesearch/advanced_search" = false;


(* Test that we actually obay sites of our own User-agent * / 'webanalyzer' *)
(* From first '*' *)
val testIsPathDisallowed031 = Robots.isPathAllowed "/?" = false;
val testIsPathDisallowed032 = Robots.isPathAllowed "/?a" = false;
val testIsPathDisallowed033 = Robots.isPathAllowed "/?foo=bar&bar=foo" = false;
(* From 'webanalyzer' *)
val testIsPathDisallowed034 = Robots.isPathAllowed "/books" = false;
val testIsPathDisallowed035 = Robots.isPathAllowed "/complete" = false;
(* From last '*' *)
val testIsPathDisallowed036 = Robots.isPathAllowed "/trends/music?" = false;
val testIsPathDisallowed037 = Robots.isPathAllowed "/notebook/search?" = false;


(* Test that we obay the extended Robots definiton of Request-rate and
   Crawl-delay *) 
val testIsPathDisallowed041 = Robots.getCrawlDelay() = 10; 
(* Parse a new Robots.txt that uses Request-Rate instead *)
Robots.initRobotsTxt ( "User-agent: foo\n "
                     ^ "Disallow: /bar\n "
                     ^ "Crawl-delay: 30 "    

                     ^ "User-Agent: webanalyzer\n"
                     ^ "Disallow: /test\n"
                     ^ "Request-rate: 2/10\n"
 
                     ^ "User-Agent: bar\n"
                     ^ "Disallow: /foo\n"
                     ^ "Crawl-delay: 50");

val testIsPathDisallowed042 = Robots.getCrawlDelay() = Int.div(10,2);


