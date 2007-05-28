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
val sentenceDelimiters = explode ".!?";
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
    (* And intermediate definition of sentenceelements,
       where the WordAttributes is like those from TextExtractor.
       The only difference between these and the resulting words
       is that these can have a TextDirection specified. *)
    type word = text * TextExtractor.WordAttribute list;
    datatype SentenceElementI = WordI of word
                              | PunctuationI of text;

    (* Partitions a string in SentenceElements and applies the given attributes.
       Generally it just converts all sequences of letters to Words
       and all sequences of punctutation to Punctuations.
       Besides that, it makes three assumptions:
         1. If there is no space after a dot "." and its followed by a letter,
            it is considered to be part of an acronym.
         3. If a dot is followed by a lowercase letter, the dot is considered the
            end of an acronym.
         2. If single upper-case letter is followed by a dot, the letter is
            is considered an initial (eg. "H. Hansen").

       These assumptions makes the sentencifier better at guessing
       where sentences ends. *)
    fun wordify' _ [] _ = []
      | wordify' attrs (x::xs) preceding =
        let
            val xword = if Config.isAlphabetic x
                        then WordI (str x, attrs)
                        else PunctuationI (str x)
            val rest = wordify' attrs xs (SOME x)
        in
            case rest of
                [] => [xword]
              | (WordI (y, _)) :: ys => (case xword of
                                             WordI(z, _) => WordI (z ^ y, attrs) :: ys
                                           (* A dot followed by a letter is probably part of an acronym *)
                                           | PunctuationI "." => WordI ("." ^ y, TextExtractor.Acronym :: attrs) :: ys
                                           | _ => xword :: rest)
              | (PunctuationI y) :: ys => case xword of
                                              PunctuationI "." => 
                                            (* If the next character
                                               is lowercase, this is
                                               probably the end of an
                                               acronym *)
                                              (case List.find (fn WordI _ => true
                                                                | _       => false) ys
                                                of SOME (WordI (z, _)) => if Char.isLower (String.sub (z, 0))
                                                                          then WordI (".", attrs) :: rest
                                                                          else PunctuationI ("." ^ y) :: ys
                                                 | _ => PunctuationI ("." ^ y) :: ys)
                                            | PunctuationI z => PunctuationI (z ^ y) :: ys
                                            | WordI(z, _) => if Char.isUpper (String.sub (z, 0)) andalso
                                                                ((not o Option.isSome) preceding orelse
                                                                 (not o Char.isUpper o Option.valOf) preceding) andalso
                                                                String.sub (y, 0) = #"."
                                                             then WordI (z ^ ".", attrs) ::
                                                                  PunctuationI (String.extract (y, 1, NONE)) :: ys
                                                             else xword :: rest
        end

    (* Concatenates sequences of SentenceElements of the same type.
       Eg. the word "con" followed by the word "cat" is replaced by
       the word "concat". (The same applies for Punctuation elements) *)
    fun concatRepetitions  ((WordI (x, xattrs)) :: (WordI (y, yattrs)) :: xs) =
            (WordI (x ^ y, foldr (fn (z, b) => TextExtractor.addWordAttribute z b) xattrs yattrs))
            :: (concatRepetitions xs)
      | concatRepetitions ((PunctuationI x) :: (PunctuationI y) :: xs) =
            (PunctuationI (x ^ y)) :: (concatRepetitions xs)
      | concatRepetitions (x :: xs) = x :: concatRepetitions xs
      | concatRepetitions [] = []

    (* TextExtractor.WordAttribute list -> WordAttribute list
       Removes Bidirectional attributes. *)
    fun convertAttrs (TextExtractor.Emphasized :: xs) = Emphasized :: convertAttrs xs
      | convertAttrs (TextExtractor.Acronym :: xs) = Acronym :: convertAttrs xs
      | convertAttrs (TextExtractor.Code :: xs) = Code :: convertAttrs xs
      | convertAttrs ((TextExtractor.Bidirectional _) :: xs) = convertAttrs xs
      | convertAttrs ((TextExtractor.Language x) :: xs) = (Language x) :: convertAttrs xs
      | convertAttrs [] = []

    (* SentenceElementI -> SentenceElement
       Removes Bidirectional WordAttributes and reverses the words if specified.
     *)
    fun convertSentenceElems ((WordI (text, attrs)) :: xs) = 
        let
            open TextExtractor;
            val rtext = if (Bidirectional RightToLeft) member attrs
                        then (implode o rev o explode) text (* reverse the string *)
                        else text;
        in
            Word (rtext, convertAttrs attrs) :: convertSentenceElems xs
        end
      | convertSentenceElems ((PunctuationI x) :: xs) = Punctuation x
                                                        :: convertSentenceElems xs
      | convertSentenceElems [] = []
in
    (* Partitions text in words and applies the given attributes. *)
    fun wordify (text, attrs) = (convertSentenceElems o concatRepetitions)
                                    (wordify' attrs  (explode text) NONE)
end

local
    (* Indicates whether a SentenceElement contains a sentence-delimiter. *)
    fun sentenceDelimiter (Punctuation x) = List.exists isSentenceDelimiter
                                                        (explode x)
      | sentenceDelimiter _ = false;

    (* Splits a list of SentenceElements in sentences. *)
    fun splitInSentences [] = []
      | splitInSentences (x :: xs) =
            if sentenceDelimiter x
            then [x] :: (splitInSentences xs)
            else case (splitInSentences xs) of
                     (sentence :: sentences) => (x :: sentence) :: sentences
                   | [] => [[x]]

    (* Splits text in words and sentences *)
    val sentencify' = splitInSentences o (concatMap wordify);        
in 
    (* Splits a string in words and sentences. *)
    fun sentencifyString str = splitInSentences (wordify (str, []));


    (* Splits a TextExtractor.textelement in words an sentences. *)
    fun sentencifyTextelement (TextExtractor.Paragraph (texts, descs)) =
            Paragraph (sentencify' texts, map sentencifyString descs) 
      | sentencifyTextelement (TextExtractor.Heading x) =
            Heading (map sentencifyTextelement x)
      | sentencifyTextelement (TextExtractor.Quotation x) =
            Quotation (map sentencifyTextelement x);

    (* Splits a TextExtractor.paragraphiseddocument in sentences and 
       words. If a WordAttribute is applied to any part of the text,
       that attribute is applied to all words in that particular part. *)
    fun sentencify ({content, languagecode, title}
                    : TextExtractor.paragraphiseddocument) =
        {content = map sentencifyTextelement content,
         languagecode = languagecode,
         title = Option.map sentencifyString title} : document;
    
end;

end;
