(* Test of HTMLLexer *)
local

fun getTag (parseTree) = case parseTree of
                             HTMLParser.Tag (tag, children) => SOME (tag, children)
                           | _ => NONE
fun getText (parseTree) = case parseTree of 
                            HTMLParser.Text t => SOME t
                           | _ => NONE

val parseTree = HTMLParser.parse   ( "<html>"
                                     ^   "<head>"
                                     ^     "<title>Foo</title>"
                                     ^   "</head>"
                                     ^   "<body>"
                                     ^     "<a href=\"bas.html\">BAR</a>"
                                     ^   "</body>"
                                     ^ "</html>" );

val (html, htmlRest) = valOf (getTag (List.hd parseTree));
val (head, headRest) = valOf (getTag (List.hd htmlRest));
val (title, titleRest) = valOf (getTag (List.hd headRest));
val titleData = valOf (getText (List.hd titleRest));
val (body, bodyRest) = valOf (getTag (List.hd (List.tl htmlRest)));
val (a, aRest) = valOf (getTag (List.hd bodyRest));
val aData = valOf (getText (List.hd aRest));

in

(* Test that the parser parses the correct tags *)
val testHTMLParser001 = HTMLParser.tagName html = "html";
val testHTMLParser002 = HTMLParser.tagName head = "head";
val testHTMLParser003 = HTMLParser.tagName title = "title";
val testHTMLParser004 = HTMLParser.textContents titleData = "Foo";
val testHTMLParser005 = HTMLParser.tagName body = "body";
val testHTMLParser006 = HTMLParser.tagName a = "a";
val testHTMLParser007 = HTMLParser.textContents aData = "BAR";

(* Test that we pull out the correct attributes from tags.*)
val testHTMLParserAttributes001 = HTMLParser.getAttribute "href" a = SOME "bas.html";


(* NEEDS ALSO TO TEST FOR SUPPORT OF <br> and <hr> maby <p> *)


end;
