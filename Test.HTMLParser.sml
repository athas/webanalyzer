(* Test of HTMLLexer *)
open HTMLParser;
local

fun getTag (parseTree) = case parseTree of
                             Tag (tag, children) => SOME (tag, children)
                           | _ => NONE
fun getText (parseTree) = case parseTree of 
                             Text t => SOME t
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
val testHTMLParser001 = tagName html = "html";
val testHTMLParser002 = tagName head = "head";
val testHTMLParser003 = tagName title = "title";
val testHTMLParser004 = textContents titleData = "Foo";
val testHTMLParser005 = tagName body = "body";
val testHTMLParser006 = tagName a = "a";
val testHTMLParser007 = textContents aData = "BAR";

(* Test that we pull out the correct attributes from tags.*)
val testHTMLParserAttributes001 = getAttribute "href" a = SOME "bas.html";

end;
