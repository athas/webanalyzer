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

fun getLinks [] = []
  | getLinks htmlTree = 
    let
        fun getLinks' (ret, HTMLParser.Tag (tag, [])) = ret
          | getLinks' (ret, HTMLParser.Tag (tag, child :: rest)) =     
            if ((HTMLParser.tagName tag) = "a") then
                case (HTMLParser.getAttribute "href" tag) of
                    SOME str => getLinks' ((str :: ret), child) @ getLinks (rest)
                    (* we know this is a link tag, if we get NONE then
                       it is a ill-formed HTML tag, so just move on *)
                  | NONE => getLinks' (ret, child) @ getLinks (rest)
            else if ((HTMLParser.tagName tag) = "frame" orelse 
                     (HTMLParser.tagName tag) = "iframe") then
                case (HTMLParser.getAttribute "src" tag) of
                    SOME str => getLinks' ((str :: ret), child) @ getLinks (rest)
                    (* ill-formed HTML tag, just move on *)
                  | NONE => getLinks' (ret, child) @ getLinks (rest)
                (* None of the tags we are looking for so just move on *)
            else
                getLinks' (ret, child) @ getLinks (rest)
          | getLinks' (ret, HTMLParser.Text t) = ret
    in
        getLinks'([], List.hd htmlTree) @ getLinks (List.tl htmlTree)
    end


fun main (arg :: _) = unparse ((HTMLParser.parse o Http.getURI o Http.buildURI) (NONE, arg))
  | main [] = print "Not enough arguments\n";

val _ = main (CommandLine.arguments ());
