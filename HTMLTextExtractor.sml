structure HTMLTextExtractor :> HTMLTextExtractor =
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
                       | Bidirectional of TextDirection;

(* A text format where most of HTML's nesting is removed. *)
datatype paragraphised = Paragraph of (string * WordAttribute list) list * text list
                       | Heading of paragraphised list
                       | Quotation of paragraphised list;

local
    val headings = ["h1", "h2", "h3", "h4", "h5", "h6"];
    val emphasizing = ["strong", "em", "b", "i", "u"];
    val acronym = ["acronym", "abbr", "dfn"];

    (* Tags that causes paragraphs to end and starts a new line*)
    val block  = ["p", "div", "form", "table",
                  "ul", "ol", "li", "dl", "di",
                  "tr", "td","th",
                  "tbody", "thead", "tfoot",
                  "object", "embed", "img",
                  "noscript"] @ headings;

    (* Tags that shouldn't cause any end of paragraphs and newlines. *)
    val inline = ["span", "font", "a", "bdo", "address",
                  "blockquote", "q", "cite",
                  "var", "kbd", "samp",
                  "sub", "sup", "big", "small"]
                 @ emphasizing
                 @ acronym;

    (* Tags that has no visible content. *)
    val nonvisual = ["script", "style",
                     "colgroup", "col",
                     "param", "meta", "link"];
in
    fun isHeading tag = tag member headings;
    fun isEmphasizing tag = tag member emphasizing;
    fun isAcronym tag = tag member acronym;
    fun isBlock tag = tag member block;
    fun isInline tag = tag member inline;
    fun isNonVisual tag = tag member nonvisual;
end;

(* Get the language of a tag (i.e. the lang or xml:lang attribute values *)
fun getLanguage tag = case getAttribute "lang" tag of
                          NONE => getAttribute "xml:lang" tag
                        | SOME x => SOME x;

(* Adds a WordAttribute to a set of attributes.
   - There can be only one Language (A new language will replace an existing)
   - Only one TextDirection is allowed
   - Acronym, Code and Emphasized can only occur once. *)
fun addToAttrSet (Language x) lst = (Language x)
                                  :: (List.filter (fn (Language _) => false
                                                    | _ => true)
                                                  lst)
  | addToAttrSet (Bidirectional x) lst = (Bidirectional x)
                                       :: (List.filter (fn (Bidirectional _) => false
                                                         | _ => true)
                                                       lst)
  | addToAttrSet x lst = x :: List.filter (fn y => y <> x) lst;


local


(* Intermediate datatype used by the flatten function  *)
datatype flat = FlatText of (string * WordAttribute list)
              | FlatHeading of flat list
              | FlatQuotation of flat list
              | Description of text
              | NewParagraph;

(* Removes repeated NewParagraph's. *)
fun rmExtraParagraphs (NewParagraph :: NewParagraph :: xs) =
        rmExtraParagraphs (NewParagraph :: xs)
  | rmExtraParagraphs (x :: xs) = x :: (rmExtraParagraphs xs)
  | rmExtraParagraphs [] = [];


fun flatten' attr ((Text t) :: rest) = (FlatText ((textContents t), attr))
                                      :: (flatten' attr rest)
  | flatten' attr [] = []
  | flatten' attr ((Tag (tag, subtrees)) :: rest) =
    let
        val newattr' = case getLanguage tag of
                           SOME x => addToAttrSet (Language x) attr
                         | _ => attr;
        val newattr = case getAttribute "dir" tag of
                          SOME "rtl" => addToAttrSet (Bidirectional RightToLeft) 
                                                     newattr'
                        | SOME "ltr" => addToAttrSet (Bidirectional LeftToRight)
                                                     newattr'
                        | _ => newattr';

        val tagname = tagName tag;

        val descriptions = map (fn x => Description x)
                               (SOMEs [getAttribute "title" tag,
                                       getAttribute "alt" tag,
                                       getAttribute "summary" tag]);            
    in
        descriptions @
        (if isEmphasizing tagname
        then (flatten' (addToAttrSet Emphasized newattr)
                       subtrees)
             @ (flatten' attr rest)

        else if isAcronym tagname
        then (flatten' (addToAttrSet Acronym newattr)
                       subtrees)
             @ (flatten' attr rest)

        else if isHeading tagname
        then NewParagraph
             :: (FlatHeading (flatten' newattr subtrees))
             :: (flatten' attr rest)

        else if isBlock tagname
        then (NewParagraph :: (flatten' newattr subtrees))
             @
             (NewParagraph :: (flatten' attr rest))

        (* Should probably be handled by a filtering-module *)
        else if isNonVisual tagname
        then flatten' attr rest

        else  (* Defaults to inline *)
            (flatten' newattr subtrees)
            @ (flatten' attr rest))
    end;


fun paragraphise _ [] = [] : paragraphised list
  | paragraphise descs (NewParagraph :: rest) = (Paragraph ([], [])) :: (paragraphise descs rest)
  | paragraphise descs ((FlatText x) :: rest) = (case paragraphise descs rest of
                                                     ((Paragraph (xs, de)) :: r) => Paragraph (x::xs, de) :: r
                                                   | [] => [(Paragraph ([x], descs))]
                                                   | therest => (Paragraph ([x], descs)) :: therest)
  | paragraphise descs ((Description x) :: rest) = paragraphise (x::descs) rest
  | paragraphise descs ((FlatHeading x) :: rest) = (Heading (paragraphise [] x))
                                                   :: (paragraphise descs rest)
  | paragraphise descs ((FlatQuotation x) :: rest) = (Quotation (paragraphise [] x))
                                                     :: (paragraphise descs rest);

in

    (* flatten: parsetree list -> flat list
       
       Converts a HTML parsetree in to a flatter format. *)
    val flatten = (paragraphise []) o rmExtraParagraphs o (flatten' []);
end



local
    type word = text * WordAttribute list;
     
    (* Either a word, a space or punctuation *)
    datatype SentenceElement = Word of word
                             | Space
                             | Punctuation of text;

    fun wordify' attrs [] = []
      | wordify' attrs (x::xs) =
        let
            val xword = if Char.isAlpha x
                        then Word (str x, attrs)
                        else if Char.isSpace x
                        then Space
                        else Punctuation (str x)
            val rest = wordify' attrs xs
        in
            case rest of
                [] => [xword]
              | Space :: ys => (case xword of 
                                        Space => Space :: ys
                                      | _ => xword :: rest)
                                   
              | (Word (y, _)) :: ys => (case xword of
                                            Word(z, _) => Word (z ^ y, attrs) :: ys
                                          | _ => xword :: rest)
                                       
              | (Punctuation y) :: ys => case xword of
                                             Punctuation z => Punctuation (z ^ y) :: ys
                                           | _ => xword :: rest
        end

    fun concatRepetitions (Space :: Space :: xs) = Space :: (concatRepetitions xs)
      | concatRepetitions ((Word (x, xattrs)) :: (Word (y, yattrs)) :: xs) =
            (Word (x ^ y, foldr (fn (z, b) => addToAttrSet z b) xattrs yattrs)) :: (concatRepetitions xs)
      | concatRepetitions ((Punctuation x) :: (Punctuation y) :: xs) =
            (Punctuation (x ^ y)) :: (concatRepetitions xs)
      | concatRepetitions (x :: xs) = x :: concatRepetitions xs
      | concatRepetitions [] = []

    fun convertAttrs (Emphasized :: xs) = TextAnalyser.Emphasized :: convertAttrs xs
      | convertAttrs (Acronym :: xs) = TextAnalyser.Acronym :: convertAttrs xs
      | convertAttrs (Code :: xs) = TextAnalyser.Code :: convertAttrs xs
      | convertAttrs ((Bidirectional _) :: xs) = convertAttrs xs
      | convertAttrs ((Language x) :: xs) = (TextAnalyser.Language x) :: convertAttrs xs
      | convertAttrs [] = []

    fun convertSentenceElems (Space :: xs) = convertSentenceElems xs
      | convertSentenceElems ((Word (text, attrs)) :: xs) = 
        let
            val rtext = (case (Bidirectional RightToLeft) member attrs of
                             true => (implode o rev o explode) text
                           | false => text)
        in
            TextAnalyser.Word (rtext, convertAttrs attrs) :: convertSentenceElems xs
        end
      | convertSentenceElems ((Punctuation x) :: xs) = TextAnalyser.Punctuation x :: convertSentenceElems xs
      | convertSentenceElems [] = []
in
    fun wordify (text, attrs) = (convertSentenceElems o concatRepetitions) (wordify' attrs  (explode text))
end


fun sentencify text = 
    let
        val words = [concatMap wordify text]
    in
        words
    end
fun sentencifyTextElement (Paragraph (texts, descs)) = TextAnalyser.Paragraph (sentencify texts, descs)
  | sentencifyTextElement (Heading x) = TextAnalyser.Heading (map sentencifyTextElement x)
  | sentencifyTextElement (Quotation x) = TextAnalyser.Quotation (map sentencifyTextElement x)


fun extractBody parsetrees = ((map sentencifyTextElement) o flatten) parsetrees;

local
    (* Find all visible string content in the given parsetree list. *)
    fun stringContent' [] = ""
      | stringContent' ((Text t) :: rest) =
            (textContents t) ^ (stringContent' rest)

      | stringContent' (Tag (_, subtrees) :: rest) =
            (stringContent' subtrees) ^ (stringContent' rest);

    fun stringContent (Tag (_, subtrees)) = [stringContent' subtrees]
      | stringContent (Text t) = [textContents t]
in
    (* Gets the title of a webpage, given the head section of that
    webpage. *)
    fun getTitle [] = NONE
      | getTitle head = Option.map (sentencify
                                    o (map (fn x => (x, [])))
                                    o stringContent)
                                   (find "title" head);
end;



(* TODO: take care of framesets: analyse <noframes>-contents *)
fun extractText (alltags as (Tag (tag, subtrees) :: tags)) =
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
                                SOME (Tag (_, subtree)) => extractBody subtree
                              | _ => [];
          in
              {title = title,
               languagecode = doc_lang,
               content=content}
          end

      | _ => {title = getTitle alltags,
              languagecode = NONE,
              content = extractBody alltags})

  | extractText (Text t :: rest) =
    let
        val {title, languagecode, content} = extractText rest
    in
        {title = title,
         languagecode = languagecode,
         content = (TextAnalyser.Paragraph ([wordify (textContents t, [])], []))
                   :: content}
    end
  | extractText [] = {title = NONE, 
                    languagecode = NONE,
                    content = []}



(* 
http://www.w3.org/TR/html401/struct/global.html

TODO:

 - <title> som (sentence list) option i stedet for string option
 - placer formularer, knapper, labels
 - title, alt, summary
 - Analyser <noframes>, for framesets
 - opdel i sÅÊtninger
 - brug lister i stedet for Binaryset.set ?

Skal dette modul vende teksten i bdo's og tags med dir="rtl"? Ja
Hvad betyder <bdo dir="rtl"><bdo dir="ltr">foobar</bdo></bdo>? Det inderste tag gÅÊlder.
*)
                                    
    
end; (* structure deHTMLifier end *)



(* findElement : tag -> parsetree list -> parsetree

   Finds the first subtree in lst with tag-name x.
   Only searches the top level of given the parsetree list. 
fun findElement y [] = NONE
  | findElement y ((x as (Tag (tag, subtrees))) :: xs) =
        if y = tagName tag
        then SOME x
        else (case findElement y subtrees of
                 SOME z => SOME z
               | NONE => findElement y xs)
  | findElement y (_ :: xs) = findElement y xs;


    fun wordAttrOrder (Language _) (Language _) = EQUAL
      | wordAttrOrder Emphasized Emphasized = EQUAL
      | wordAttrOrder (Acronym _) (Acronym _) = EQUAL
      | wordAttrOrder (Bidirectional _) (Bidirectional _) = EQUAL           
      | wordAttrOrder (Language _) _ = GREATER
      | wordAttrOrder Emphasized (Language _) = LESS
      | wordAttrOrder Emphasized _ = GREATER
      | wordAttrOrder (Acronym _) (Bidirectional _) = LESS
      | wordAttrOrder (Acronym _) _ = GREATER
      | wordAttrOrder (Bidirectional _) _ = GREATER

    fun wordAttrOrderPair (x, y) = wordAttrOrder x y;

 *)
