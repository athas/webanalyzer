open TextIO;

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

fun unparse tags = (map unparse' tags; ());

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
                    then Http.buildURI(NONE, path)
                    else Http.buildURI(absoluteURI, path)

                (* Scan the tags attribute for its known link 'attr'
                   and returns the attribute data *)
                fun checkAttribute (ret, tag, attr, chilren) =
                    case (HTMLParser.getAttribute attr tag) of
                        SOME str => if String.isPrefix "mailto:" str
                                    then getChildren' (ret, children)
                                    else getChildren' ((makeURI str) :: ret, children)
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


val getAndParse = (HTMLParser.parse o Http.getURI)

fun main (arg :: _) = 
    let val uri = Http.buildURI (NONE, arg)
        val robotsuri = Http.buildURI (NONE, Http.protocolFromURI uri
                                             ^ "://"
                                             ^ Http.serverFromURI uri
                                             ^ "/robots.txt")
        val robotstxt = (Http.getURI robotsuri) 
            handle Http.Error (Http.HTTP (404, _)) => ""
    in Robots.initRobotsTxt robotstxt;
       map (fn link => print (Http.stringFromURI link) before print "\n") (getLinks (getAndParse uri) (SOME uri))
       handle Http.Error (Http.HTTP (404, _)) => []
            | Http.Error (Http.General s) => (print s; print "\n"; raise Fail "General")
            | Http.Error (Http.Socket s) => (print s; print "\n"; raise Fail "");
       flushOut stdOut
    end
  | main [] = print "Not enough arguments\n";

(*val _ = main (CommandLine.arguments ());*)
