structure Sentencifier :> Sentencifier =
struct

open Util;
infix 0 member;

type text = TextExtractor.text;

(* Attributes that a word can have *)
datatype WordAttribute = Language of text
                       | Emphasized
                       | Code (* var, kbd *)
                       | Acronym;

type word = text * WordAttribute list;
     
(* Either a word or punctuation.

   [From Wikipedia] Punctuation is everything in written language
   other than the actual letters, including punctuation marks,
   inter-word spaces, capitalization, and indentation (Todd, 2000). *)
datatype SentenceElement = Word of word
                         | Punctuation of text;

(* A list of sentence elements, there should be space between each
element. *)
type sentence = SentenceElement list;

datatype textelement = Paragraph of sentence list *
                                    sentence list list
                     | Heading of textelement list
                     | Quotation of textelement list;
                 (*  | Code of text (* <code> *) *)

type document = {title : sentence list option,
                 languagecode : string option,
                 content : textelement list};

(* Characters that ends a sentence *)
val sentenceDelimiters = explode ".:!?";
fun isSentenceDelimiter x = x member sentenceDelimiters;


(* Adds a WordAttribute to a set of attributes.
   - There can be only one Language (A new language will replace an existing)
   - Acronym, Code and Emphasized can only occur once. *)
fun addToAttrSet (Language x) lst =
        (Language x)
        :: (List.filter (fn (Language _) => false
                          | _ => true)
                        lst)
  | addToAttrSet x lst = x :: List.filter (fn y => y <> x) lst;


local
    type word = text * TextExtractor.WordAttribute list;
     
    (* Either a word, a space or punctuation.

       We need to save the Spaces to find words splitted in two by a
       HTML-tag, like: f<strong>oo</strong>bar  *)
    datatype SentenceElementI = WordI of word
                              | PunctuationI of text;

    fun wordify' attrs [] = []
      | wordify' attrs (x::xs) =
        let
            val xword = if Config.isAlphabetic x
                        then WordI (str x, attrs)
                        else PunctuationI (str x)
            val rest = wordify' attrs xs
        in
            case rest of
                [] => [xword]
              | (WordI (y, _)) :: ys => (case xword of
                                            WordI(z, _) => WordI (z ^ y, attrs) :: ys
                                          | _ => xword :: rest)
                                       
              | (PunctuationI y) :: ys => case xword of
                                              PunctuationI z => PunctuationI (z ^ y) :: ys
                                            | _ => xword :: rest
        end

    fun concatRepetitions  ((WordI (x, xattrs)) :: (WordI (y, yattrs)) :: xs) =
            (WordI (x ^ y, foldr (fn (z, b) => TextExtractor.addWordAttribute z b) xattrs yattrs))
            :: (concatRepetitions xs)
      | concatRepetitions ((PunctuationI x) :: (PunctuationI y) :: xs) =
            (PunctuationI (x ^ y)) :: (concatRepetitions xs)
      | concatRepetitions (x :: xs) = x :: concatRepetitions xs
      | concatRepetitions [] = []

    fun convertAttrs (TextExtractor.Emphasized :: xs) = Emphasized :: convertAttrs xs
      | convertAttrs (TextExtractor.Acronym :: xs) = Acronym :: convertAttrs xs
      | convertAttrs (TextExtractor.Code :: xs) = Code :: convertAttrs xs
      | convertAttrs ((TextExtractor.Bidirectional _) :: xs) = convertAttrs xs
      | convertAttrs ((TextExtractor.Language x) :: xs) = (Language x) :: convertAttrs xs
      | convertAttrs [] = []

    fun convertSentenceElems ((WordI (text, attrs)) :: xs) = 
        let
            val rtext =
                    (case (TextExtractor.Bidirectional TextExtractor.RightToLeft) member attrs of
                         true => (implode o rev o explode) text (* reverse the string *)
                       | false => text);
        in
            Word (rtext, convertAttrs attrs) :: convertSentenceElems xs
        end
      | convertSentenceElems ((PunctuationI x) :: xs) = Punctuation x
                                                        :: convertSentenceElems xs
      | convertSentenceElems [] = []
in
    fun wordify (text, attrs) = (convertSentenceElems o concatRepetitions)
                                    (wordify' attrs  (explode text))
end

local

    fun sentenceDelimiter (Punctuation x) = List.exists isSentenceDelimiter
                                                        (explode x)
      | sentenceDelimiter _ = false;

    fun splitInSentences [] = []
      | splitInSentences (x :: xs) =
            if sentenceDelimiter x
            then [x] :: (splitInSentences xs)
            else case (splitInSentences xs) of
                     (sentence :: sentences) => (x :: sentence) :: sentences
                   | [] => [[x]]
in 
    fun sentencify text = 
        let
            val words = concatMap wordify text
        in
            splitInSentences words
        end;

    fun sentencifyString str = splitInSentences (wordify (str, []));
end


fun sentencifyTextelement (TextExtractor.Paragraph (texts, descs)) =
        Paragraph (sentencify texts, map sentencifyString descs)
  | sentencifyTextelement (TextExtractor.Heading x) =
        Heading (map sentencifyTextelement x)
  | sentencifyTextelement (TextExtractor.Quotation x) =
        Quotation (map sentencifyTextelement x);

fun sentencifyParagraphised ({content, languagecode, title}
                                : TextExtractor.paragraphiseddocument) =
        {content = map sentencifyTextelement content,
         languagecode = languagecode,
         title = Option.map sentencifyString title} : document;
end
