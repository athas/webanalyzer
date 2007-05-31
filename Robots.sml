(* 

   Functions for parsing robots robots.txt and then query for allowed paths 
  
   For reference, look at: http://www.robotstxt.org/wc/norobots.html 

*)

structure Robots :> Robots =
struct

(* reference string used to hold the disallowed paths from robots.txt, 
   so it can be search later *)
val disallowedPaths : string list ref = ref [];

(* de-refs disallowedPaths. Used when searching by 'isPathAllowed' *)
fun getDisallowedPaths() = !disallowedPaths;

(* Resets the disallowedPaths list *)
fun clearRobotsTxt () = (
    disallowedPaths := [];
    Config.setCrawlDelay 0;
    () );
    
(* Initializes the 'disallowedPaths' from the content of a robots.txt *)
fun initRobotsTxt robotsStr = 
    let
        (* break up in user agents (InCase Sensitive *)
        val regex = RegexMatcher.compileString
        val userAgentsRegexp = regex "[uU][sS][eE][rR]-[aA][gG][eE][nN][tT]:"
        val userAgents = Util.regexTokens userAgentsRegexp robotsStr

        (* remove user agents not of interest for this crawler *)
        fun filterOtherUserAgents strLst =
            let
                fun filterOtherUserAgents' ret [] = ret
                  | filterOtherUserAgents' ret (strLst as s::ss) =
                    let
                        (* extract the user-agent string, which is the
                           first string until the first \n in the list *)
                        val userAgent = Util.trimStr (String.substring 
                                                          (s, 0, Util.getFirstIndexOf(#"\n", s))
                                                      handle Subscript => "");
                    in
                        (* match the userAgent against this crawlers own
                           name and * which applys to all *)
                        if ((userAgent = "*") orelse (userAgent = Config.httpUserAgent ())) then
                            filterOtherUserAgents' ((s) :: ret) (ss)
                        else filterOtherUserAgents' ret ss
                    end
            in
                clearRobotsTxt ();
                filterOtherUserAgents' [] strLst
            end

        (* String list containing info from Robots.txt that is of interest for our webcrawler *)
        val useableAgents = filterOtherUserAgents userAgents;

        (* Takes a String list and removes all information except
           the paths that follows from a 'Disallow:' *)
        fun makeDisallowLst strLst =
            let
                (* Set the crawlDelay from crawlDelay in robots.txt *)
                fun handleCrawlDelay str = case Int.fromString str of
                                               SOME n => Config.setCrawlDelay n
                                             | NONE => ()
                                     
                (* Set the crawlDelay from the requestRatein in robots.txt *)
                fun handleRequestRate str = 
                    let
                        val splitStr = String.tokens (fn str => str = #"/") str 
                        fun handleRequestRate' (numPages::inTime::[]) = 
                            let
                                val numPages' = Int.fromString numPages
                                val inTime' = Int.fromString inTime
                            in
                                if (Option.isSome numPages' andalso Option.isSome inTime') then
                                    Config.setCrawlDelay (Int.div(valOf inTime', valOf numPages'))
                                else ()
                            end
                          | handleRequestRate' _ = ()
                    in  
                        if (List.length splitStr = 2) then
                            handleRequestRate' splitStr
                        else
                            ()         
                    end
              
                (* regexp expressions for matching string in the robots.txt *)
              
                fun makeDisallowLst' ret [] = ret
                  | makeDisallowLst' ret [_] = ret
                  | makeDisallowLst' ret (s::(strTail as ss::sss)) = 
                    (* if s matches one of the above Regexp then process the next
                       and move on to the 2. next and start over.
                       If this doesn't match the above  Regexps then
                       just to the next element check that one. *)
                    if Util.equalICase "disallow:" s then
                        makeDisallowLst' (ss :: ret) sss 
                    else if Util.equalICase "crawl-delay:" s then
                        makeDisallowLst' ret sss before handleCrawlDelay ss
                    else if Util.equalICase "request-rate:" s then
                        makeDisallowLst' ret sss before handleRequestRate ss
                    else
                        makeDisallowLst' ret strTail
            in
                makeDisallowLst' [] strLst
            end
            
        (* Concatenates the disallowed paths from the string list list *)
        val disallowLst = List.concat 
                               (* keeps all paths that is after a "disallow:" in the string list.*)
                              (List.map makeDisallowLst 
                                        (* splits each string in the useableAgents list up 
                                            to a string list according to spaces and newlines 
                                            (Char.isSpace) *)
                                        (List.map (fn xs => String.tokens Char.isSpace xs) 
                                                  useableAgents));
    in
        (* save the final disallow lis in a public variable so 
           it can be assesed later by the query function *)
        disallowedPaths := disallowLst
    end

(* Scans the disallowedPaths, that was created at by initRobotsTxt
   and check that no elements prefixes the given path *)
fun isPathAllowed path = 
        not (List.exists (fn disallowedPath => 
                             (String.isPrefix disallowedPath path))
                         (getDisallowedPaths()))


end;
