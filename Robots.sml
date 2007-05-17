(* Functions for parsing robots robots.txt and then query for allowed paths *)

val disallowedPaths : string list ref = ref [];

    
(* http://www.robotstxt.org/wc/norobots.html *)

fun initRobotsTxt robotsStr = 
    let
        (* break up in user agents and convert the substring list to string list *)
        val userAgents = List.map Substring.string 
                                  (Regex.tokens 
                                       (Regex.regcomp "user-agent:" [Regex.Icase]) 
                                       robotsStr);

        (* remove user agents not of interest for this crawler *)
        fun filterOtherUserAgents strLst =
            let
                fun filterOtherUserAgents' ret [] = ret
                  | filterOtherUserAgents' ret (strLst as s::ss) =
                    let
                        
                        (* extract the user-agent string, which is the
                           first string until the first \n in the list *)
                        val userAgent = trimStr (String.substring(s, 
                                                                 0, 
                                                                 getFirstIndexOf(#"\n", 
                                                                                 s)));
                    in
                        (* match the userAgent against this crawlers own
                           name and * which applys to all *)
                        if ((userAgent = "*") orelse (userAgent = programUserAgent)) then
                            filterOtherUserAgents' ((s) :: ret) (ss)
                        else
                            filterOtherUserAgents' ret ss
                    end

                    
            in
              filterOtherUserAgents' [] strLst  
            end

        val useableAgents = filterOtherUserAgents userAgents;

        fun makeDisallowLst strLst =
            let
                fun makeDisallowLst' ret [] = ret
                  | makeDisallowLst' ret (strLst as s::ss) = 
                    if Regex.regexecBool 
                           (Regex.regcomp "Disallow:" [Regex.Icase]) 
                           [] 
                           s
                    then
                        (* if List.hd strLst is 'Disallow:' then save the next
                           and search further in the 2. next for 'disallow'
                           statement. If this aint a 'Disallow' statement then
                           just move further to the next and check that one. *)
                        makeDisallowLst' (List.nth(strLst, 1) :: ret) (List.tl ss) 
                        handle _ => makeDisallowLst' ret []
                    else
                        makeDisallowLst' ret (ss)
            in
                makeDisallowLst' [] strLst
            end
            
        (* Concatenates the disallowed paths from the string list list *)
        val disallowLst = List.concat 
                              (
                               (* keeps all paths that is after a "disallow:" in the string list.*)
                               List.map makeDisallowLst 
                                        (
                                         (* splits each string in the useableAgents list up 
                                            to a string list according to spaces and newlines 
                                            (Char.isSpace) *)
                                         List.map (fn xs => String.tokens Char.isSpace xs) 
                                                  useableAgents
                                        )
                              );
    in
        disallowedPaths := disallowLst;
        disallowLst
        
    (* save the final disallow lis in a public variable so 
       it can be assesed later by the query function *)

    end

fun getDisallowedPaths() = !disallowedPaths

(* Scans the disallowedPaths, that was created at by initRobotsTxt
   and check that no elements prefixes the given path *)
fun isPathAllowed path = 
    not 
        (
         List.exists 
             ( fn disallowedPath => (String.isPrefix disallowedPath path) )
             (getDisallowedPaths())
        )
    
