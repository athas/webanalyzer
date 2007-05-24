structure TextExtractor :> TextExtractor =
struct

open HTMLParser;
open Util;
infix 0 member;

type text = string


datatype TextDirection = RightToLeft
                       | LeftToRight;

(* Attributes that a word can have *)
datatype WordAttribute = Language of text
                       | Emphasized
                       | Code (* var, kbd *)
                       | Acronym
                          (* Reverse word before spellchecking? *)
                       | Bidirectional of TextDirection;

(* A text format where most of HTML's nesting is removed. *)
datatype paragraphised = Paragraph of (text * WordAttribute list) list
                                      * text list
                       | Heading of paragraphised list
                       | Quotation of paragraphised list;

(* A document partitioned in paragraphs *)
type paragraphiseddocument = {title : text option,
                              languagecode : text option,
                              content : paragraphised list};

(* http://www.w3.org/TR/html401/struct/global.html *)
local
    val headings = ["h1", "h2", "h3", "h4", "h5", "h6"];
    val emphasizing = ["strong", "em", "b", "i", "u"];
    val quotations = ["blockquote", "q"];
    val acronym = ["acronym", "abbr", "dfn"];
    val code = ["var", "kbd", "code", "samp"];
    val descriptionAttributes = ["title", "alt", "summary"];

    (* Tags that causes paragraphs to end and starts a new line*)
    val block  = ["p", "div", "form", "table",
                  "ul", "ol", "li", "dl", "di",
                  "tr", "td","th",
                  "tbody", "thead", "tfoot",
                  "object", "embed", "img",
                  "noscript"]
                 @ headings
                 @ quotations;

    (* Tags that shouldn't cause any end of paragraphs and newlines. *)
    val inline = ["span", "font", "a", "bdo", "address", "center",
                  "cite",
                  "sub", "sup", "big", "small"]
                 @ emphasizing
                 @ acronym
                 @ code;

    (* Tags that has no visible content. *)
    val nonvisual = ["script", "style",
                     "colgroup", "col",
                     "param", "meta", "link"];

in
    fun isHeading tag = tag member headings;
    fun isQuotation tag = tag member quotations;
    fun isEmphasizing tag = tag member emphasizing;
    fun isCode tag = tag member code;
    fun isAcronym tag = tag member acronym;
    fun isBlock tag = tag member block;
    fun isInline tag = tag member inline;
    fun isNonVisual tag = tag member nonvisual;
    fun isDescription attr = attr member descriptionAttributes;
end;

(* Get the language of a tag (i.e. the lang or xml:lang attribute values *)
fun getLanguage tag = case getAttribute "lang" tag of
                          NONE => getAttribute "xml:lang" tag
                        | SOME x => SOME x;

(* Adds a WordAttribute to a list of attributes.
                                    
   No attribute can occur twice, if an attribute of the type already
   already exist, the old attribute is removed and the new is used. *)
fun addWordAttribute (Language x) lst =
        (Language x)
        :: (List.filter (fn (Language _) => false
                          | _ => true)
                        lst)
  | addWordAttribute (Bidirectional x) lst =
        (Bidirectional x)
        :: (List.filter (fn (Bidirectional _) => false
                          | _ => true)
                        lst)
  | addWordAttribute x lst = x :: List.filter (fn y => y <> x) lst;

local
    (* Intermediate datatype used by the flatten function  *)
    datatype flat = FlatText of (string * WordAttribute list)
                  | FlatHeading of flat list
                  | FlatQuotation of flat list
                  | Description of text
                  | NewParagraph;

    (* Converts a parsetree to the flat format above. *)
    fun flatten' attr ((Text t) :: rest) = (FlatText ((textContents t), attr))
                                           :: (flatten' attr rest)
      | flatten' attr [] = []
      | flatten' attr ((Tag (tag, subtrees)) :: rest) =
        let
            val newattr' = case getLanguage tag of
                               SOME x => addWordAttribute (Language x) attr
                             | _ => attr;
            val newattr'' = case getAttribute "dir" tag of
                                SOME "rtl" => addWordAttribute
                                                  (Bidirectional RightToLeft) 
                                                  newattr'
                              | SOME "ltr" => addWordAttribute
                                                  (Bidirectional LeftToRight)
                                                  newattr'
                              | _ => newattr';
            val tagname = tagName tag;
            val newattr = if isEmphasizing tagname
                          then addWordAttribute Emphasized newattr''
                          else if isAcronym tagname
                          then addWordAttribute Acronym newattr''
                          else if isCode tagname
                          then addWordAttribute Code newattr''
                          else newattr'';
                
            val descriptionValues =
                SOMEs (mapAttributes (fn (t, v) => if isDescription tagname
                                                   then SOME v
                                                   else NONE)
                                     tag);
                
            val descriptions = map (fn x => Description x) descriptionValues;
                
            val flatAfter = flatten' attr rest;
            val flatContent = flatten' newattr subtrees;
        in
            (* Add the descriptions of the tag to the result *)
            descriptions
            @
            (* A Heading starts a new paragraph *)
            (if isHeading tagname   
             then NewParagraph :: (FlatHeading flatContent) :: flatAfter
                  
             (* Quotations starts and ends with new paragraphs *)
             else if isQuotation tagname
             then [NewParagraph,
                   FlatQuotation flatContent,
                   NewParagraph] @ flatAfter

             (* Other block elements starts and ends with a new paragraph *)
             else if isBlock tagname 
             then (NewParagraph :: flatContent)
                  @
                  (NewParagraph :: flatAfter)

             (* Ignore content, should have been caught by the HTMLFilter. *)
             else if isNonVisual tagname
             then flatAfter

             else  (* Defaults to inline *)
                 flatContent @ flatAfter)
        end;

    (* Removes repeated NewParagraph's. flatten' can create output with
       additional NewParagraphs, which will lead to empty paragraphs. 
  
       The nested paragraphs often occurs where nested block elements is
       found. Example:
                 <div><div>foobar</div></div>
              Gives: 
                 NewParagraph, NewParagraph, "foobar", NewParagraph,
                 NewParagraph
     *)
    fun rmExtraParagraphs (NewParagraph :: NewParagraph :: xs) =
        rmExtraParagraphs (NewParagraph :: xs)
      | rmExtraParagraphs (x :: xs) = x :: (rmExtraParagraphs xs)
      | rmExtraParagraphs [] = [];

    (* Put the results in a better datastructure, with Paragraph instead of
       NewParagraph and FlatText *)
    fun paragraphise [] = [] : paragraphised list
      | paragraphise (NewParagraph :: rest) = (Paragraph ([], []))
                                              :: (paragraphise rest)
      | paragraphise ((FlatText x) :: rest) =
            (case paragraphise rest of
                 ((Paragraph (xs, descs)) :: r) => (Paragraph (x::xs, descs))
                                                   :: r
               | [] => [(Paragraph ([x], []))]
               | therest => (Paragraph ([x], [])) :: therest)
      | paragraphise ((Description x) :: rest) =
            (case paragraphise rest of
                 ((Paragraph (xs, descs)) :: r) => Paragraph (xs, x::descs)
                                                   :: r
               | [] => [(Paragraph ([], [x]))]
               | therest => (Paragraph ([], [x])) :: therest)
      | paragraphise ((FlatHeading x) :: rest) = (Heading (paragraphise x))
                                                 :: (paragraphise rest)
      | paragraphise ((FlatQuotation x) :: rest) = (Quotation (paragraphise x))
                                                   :: (paragraphise rest);
in
    (* flatten: parsetree list -> paragraphised list
       
       Converts a HTML parsetree in to a flatter format. *)
    val flatten = paragraphise o rmExtraParagraphs o (flatten' []);
end

local
    (* Find all visible string content in the given parsetree list. *)
    fun stringContent (Tag (_, subtrees)) =
            foldl (fn (x, b) => b ^ stringContent x)
                  ""
                  subtrees
      | stringContent (Text t) = textContents t;
in
    (* Gets the title of a webpage, given the head section of that
    webpage. *)
    fun getTitle [] = NONE
      | getTitle head = Option.map stringContent
                                   (find "title" head);
end;

(* Locate the html, head and body (or noframes) and extract the text 
   of document. *)
fun extractFromHTML (alltags as (Tag (tag, subtrees) :: tags)) =
    (case tagName tag of
        "html" =>
          let
              val head = find "head" subtrees;

              (* framesets doesn't have a body, looks for noframes
              when no body is present *)
              val body = case find "body" subtrees of
                           SOME x => SOME x
                         | NONE => find "noframes" subtrees;
              (* Look for the title if the head was found *)
              val title = case head of
                              SOME (Tag (_, subtree)) => getTitle subtree
                            | _ => NONE;

              val doc_lang = getLanguage tag;

              val content = case body of
                                SOME (Tag (_, subtree)) => flatten subtree
                              | _ => [];
          in
              {title = title,
               languagecode = doc_lang,
               content=content}
          end

      | _ => {title = getTitle alltags,
              languagecode = NONE,
              content = flatten alltags})

  | extractFromHTML (Text t :: rest) =
    let
        val {title, languagecode, content} = extractFromHTML rest
    in
        {title = title,
         languagecode = languagecode,
         content = (Paragraph ([(textContents t, [])], []))
                   :: content}
    end
  | extractFromHTML [] = {title = NONE, 
                          languagecode = NONE,
                          content = []};
end;
