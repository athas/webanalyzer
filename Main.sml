open TextIO;
open Http;
open List;

local
    fun unparse' (HTMLParser.Tag (tag, children)) =
        let
            fun printChildren [] = []
              | printChildren (child :: rest) =
                (unparse' child;
                 print "\n";
                 printChildren rest);
            fun printAttribute (key, value) = (print " ";
                                               print key;
                                               print "=\"";
                                               print value;
                                               print "\"")
        in
            print "<";
            print (HTMLParser.tagName tag);
            HTMLParser.mapAttributes printAttribute tag;
            print ">\n";
            printChildren children;
            print "</";
            print (HTMLParser.tagName tag);
            print ">\n"
        end
      | unparse' (HTMLParser.Text text) = print (HTMLParser.textContents text);

in

fun unparse tags = (map unparse' tags; flushOut stdOut);

end

fun getLinks htmlTree absoluteURI = 
    let
        (* takes a parsetree list and processes down it by calling
           getLinks' on the children of the tree *)
        fun getChildren' (ret, []) = ret
          | getChildren' (ret, (child :: rest)) =
            getLinks' (ret, child) @ getChildren' ([], rest)
    
        (* Checks each parsetree for Tag's and if the tags are what
           wee looks for then it scans the tags known link attributes
           and adds then return all these links. 

           'AND' because these to functions are mutually recursive *)
        and getLinks' (ret, HTMLParser.Text t) = ret
          | getLinks' (ret, HTMLParser.Tag (tag, [])) = ret
          | getLinks' (ret, HTMLParser.Tag (tag, children)) =     
            let
                (* if the found path is relative ie. don't start with
                   http:// or www. then prefix it with the given
                   relativePath so we always get a absolute path to
                   follow. And also return it as a URI *)
                fun makeURI path =
                    if (String.isPrefix "http://" path)
                    then buildURI(NONE, path)
                    else buildURI(absoluteURI, path)

                (* Scan the tags attribute for its known link 'attr'
                   and returns the attribute data *)
                fun checkAttribute (ret, tag, attr, chilren) =
                    case (HTMLParser.getAttribute attr tag) of
                        SOME str => if String.isPrefix "mailto:" str
                                    then getChildren' (ret, children)
                                    else (getChildren' ((makeURI str) :: ret, children) 
                                          handle Http.Error (General _) => getChildren' (ret, children))
                        (* NONE means ill formed HTML tag so just move on *)
                      | NONE => getChildren' (ret, children) 
            in
                case HTMLParser.tagName tag of
                    "a" => checkAttribute(ret, tag, "href", children)
                  | "frame" => checkAttribute(ret, tag, "src", children)
                  | "iframe" => checkAttribute(ret, tag, "src", children)
                  (* If none of the above cases just move on to next tag in the parsetree *)
                  | _ => getChildren' (ret, children)
            end;
    in
        getChildren'([], htmlTree)
    end

val getAndParse = (HTMLParser.parse o getURI)

fun findStartURI URI = if Robots.isPathAllowed (stringFromURI URI)
                       then URI
                       else let val rootURI = buildURI (SOME URI, "/") in
                                print (stringFromURI URI);
                                print " is off-limits to crawlers, trying ";
                                print (stringFromURI rootURI);
                                "instead.\n";
                                rootURI
                            end

val visitedPages : URI list ref = ref [];

fun visit maxdepth uri depth = 
    if depth >= maxdepth orelse 
       contentTypeFromURI uri <> "text/html" orelse
       exists (fn x => x = uri) (!visitedPages) 
    then ()
    else (visitedPages := uri :: !visitedPages;
          print "Visiting ";
          print (stringFromURI uri);
          print "\n";
          flushOut stdOut;
          map (fn link => (print "Seeing ";
                           print (stringFromURI link);
                           print "\n";
                           flushOut stdOut;
                           visit maxdepth link (depth + 1)))
              (getLinks (getAndParse uri) (SOME uri));
          ())
         handle Error (HTTP (code, _)) => ()
              | Error (General s) => (print s; print "\n"; raise Fail "General")
              | Error (Socket s) => (print s; print "\n"; raise Fail "Socket");

val standardMaxDepth = 1;

fun main (arg :: rest) = 
    let val uri = buildURI (NONE, arg)
        val robotsuri = buildURI (NONE, protocolFromURI uri
                                        ^ "://"
                                        ^ serverFromURI uri
                                        ^ "/robots.txt")
        val robotstxt = (getURI robotsuri)
            handle Error (HTTP (404, _)) => ""
        val _ = Robots.initRobotsTxt robotstxt;
        val starturi = findStartURI uri
    in 
        (* Useful when used interactively. *)
        visitedPages := [];
        case rest of
            (maxdepth :: _) => visit (Option.valOf (Int.fromString maxdepth)) starturi 0
          | _ => visit standardMaxDepth starturi 0;
        print "Done!\n";
        flushOut stdOut
    end
  | main [] = print "Not enough arguments\n";

(*val _ = main (CommandLine.arguments ());*)
