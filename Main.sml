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

fun main (arg :: _) = unparse ((HTMLParser.parse o Http.getURI o Http.buildURI) (NONE, arg))
  | main [] = print "Not enough arguments\n";

val _ = main (CommandLine.arguments ());
