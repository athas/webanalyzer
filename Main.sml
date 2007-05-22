open TextIO;
open Http;
open HTMLParser;
open List;

local
    fun unparse' (Tag (tag, children)) =
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
            print (tagName tag);
            mapAttributes printAttribute tag;
            print ">\n";
            printChildren children;
            print "</";
            print (tagName tag);
            print ">\n"
        end
      | unparse' (Text text) = print (textContents text);

in

fun unparse tags = (map unparse' tags; flushOut stdOut);

end

fun mapLinks function htmlTree =
    let
        (* takes a parsetree list and processes down it by calling
           mapLinks' on the children of the tree *)
        fun getChildren' (ret, []) = ret
          | getChildren' (ret, (child :: rest)) =
            getLinks' (ret, child) @ getChildren' ([], rest)
    
        (* Checks each parsetree for Tag's and if the tags are what
           we looks for then it scans the tags known link attributes
           and adds then return all these links. 

           'AND' because these to functions are mutually recursive *)
        and getLinks' (ret, Text t) = ret
          | getLinks' (ret, Tag (tag, [])) = ret
          | getLinks' (ret, Tag (tag, children)) =     
                case tagName tag of
                    "a" => function tag :: getChildren' (ret, children)
                  | "frame" => function tag :: getChildren' (ret, children)
                  | "iframe" => function tag :: getChildren' (ret, children)
                  (* If none of the above cases just move on to next tag in the parsetree *)
                  | _ => getChildren' (ret, children)
    in
        getChildren'([], htmlTree)
    end

fun getLinks htmlTree absoluteURI = 
    let
        fun makeURI path =
            if (String.isPrefix "http://" path)
            then buildURI (NONE, path)
            else buildURI (absoluteURI, path)

        fun valid "a" link = not (String.isPrefix "mailto:" link)
          | valid tagname link = true

        fun checkAttribute tag attribute =
            case getAttribute attribute tag of
                SOME str => if valid (tagName tag) str then
                                (SOME (makeURI str) handle Http.Error (General _) => NONE)
                            else NONE
              | NONE => NONE
                         
        fun maybeGetLink "a" tag = checkAttribute tag "href"
          | maybeGetLink "frame" tag = checkAttribute tag "src"
          | maybeGetLink "iframe" tag = checkAttribute tag "src"
          | maybeGetLink _ _ = NONE
            
    in
        map valOf (filter Option.isSome (mapLinks (fn tag => maybeGetLink (tagName tag) tag) htmlTree))
    end

val getAndParse = (parse o getURI)

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
