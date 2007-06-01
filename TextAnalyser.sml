structure TextAnalyser :> TextAnalyser =
struct

open Util;
infix 0 member;

type text = Sentencifier.text;

(* Result of a single word,
   the boolean indicates whether the word is spelled correctly or not. *)
datatype SentenceElementResult = WordResult of text * bool
                               | PunctuationResult of text;

datatype AnalysisResult = Lix of real
                        | FleshReadingEase of real
                        | FleshKincaidGradeLevel of real;

type results = {lix : real,
                fleshReadingEase : real,
                fleshKincaidGradeLevel : real};

(* Results of sentences *)
type sentenceresult = results * (SentenceElementResult list);

(* Results from a paragraph, a heading or a quotation *)
datatype textresult = ParagraphResult of results *
                                         sentenceresult list *
                                         sentenceresult list list
                    | HeadingResult of results *
                                       textresult list
                    | QuotationResult of results *
                                         textresult list;
(* All results of a document *)
type documentresult = {titleResults : (results * sentenceresult list) option,
                       documentResults : results,
                       contentResults : textresult list};


fun getLix (results : results) = #lix results;
fun getFRE (results : results) = #fleshReadingEase results;
fun getFKGL (results : results) = #fleshKincaidGradeLevel results;
fun getBadnessFactor (results : results) = (getLix results + (100.0 - (getFRE results / 3.0))) / 4.0;

fun titleResults (documentresult : documentresult) = #titleResults documentresult;
fun documentResults (documentresult : documentresult) = #documentResults documentresult;
fun contentResults (documentresult : documentresult) = #contentResults documentresult;

(* A record containing information about a document, paragraph or sentence.
    - paragraphs is 0 and sentences is 1 for a sentence
    - paragraphs is 1 for a paragraph
 *)
type counts = {paragraphs : int,
               sentences : int,
               words : int,
               (* words with more than 6 chars, for LIX calculation *)
               longwords : int,
               vowels : int};

datatype textcounts = ParagraphCount of counts *
                                        (counts * Sentencifier.sentence) list *
                                        (counts * Sentencifier.sentence) list list
                    | HeadingCount of counts *
                                      textcounts list
                    | QuotationCount of counts *
                                        textcounts list;

val zeroCount = {paragraphs = 0,
                 sentences = 0,
                 words = 0,
                 longwords = 0,
                 vowels = 0};

fun calcCounts operator
               ({paragraphs, sentences, words, longwords, vowels} : counts)
               ({paragraphs = paragraphs2, sentences = sentences2, words = words2, 
                 longwords = longwords2, vowels = vowels2} : counts) = 
    {paragraphs = operator (paragraphs, paragraphs2),
     sentences = operator (sentences, sentences2),
     words = operator (words, words2), 
     longwords = operator (longwords, longwords2),
     vowels = operator (vowels, vowels2)} : counts;

fun addCounts (x : counts ,y : counts) = calcCounts op+ x y;
val sumCounts = foldl addCounts zeroCount;

fun countVowels str = foldl (fn (char, b) => if Config.isVowel char
                                             then 1 + b
                                             else b)
                            0 
                            (explode str);

fun isLongword str = size str > 6;

fun countSentence (sentence : Sentencifier.SentenceElement list) = 
    let
        fun countSentenceElement (Sentencifier.Punctuation _) = zeroCount
          | countSentenceElement (Sentencifier.Word (str, _)) =
            {vowels = countVowels str,
             words = 1,
             longwords = if isLongword str
                         then 1
                         else 0,
            sentences = 0,
            paragraphs = 0} : counts;

        val {words, longwords, vowels, ...} =
                sumCounts (map countSentenceElement sentence);
    in
        {paragraphs = 0,
         sentences = 1,
         words = words,
         longwords = longwords,
         vowels = vowels} : counts
    end;
    

fun countSentences sentences = let val counts = map countSentence sentences;
                                   val sum = sumCounts counts;
                               in  (sum,
                                    ListPair.zip (counts, sentences))
                               end
                                   

fun countOfTextCounts (ParagraphCount (c, _, _)) = c
  | countOfTextCounts (HeadingCount (c, _)) = c
  | countOfTextCounts (QuotationCount (c, _)) = c;


fun countTextElement (Sentencifier.Paragraph (sentences, descs)) =
    let
        val (sentencesCountSum,
             sentencesCountResults) = countSentences sentences;

        (* Descriptions is lists of lists of sentences, speciel care is
           taken *)
        val descriptionCounts = map countSentences descs
        val (descsSums,
             descsCountResults) = ListPair.unzip descriptionCounts;

        val descsSum = sumCounts descsSums

(*        val descriptionCounts : counts list = map (sumCounts o (map countSentence)) descs;
        val descriptionCountResults : (counts * Sentencifier.sentence) list list =
            ListPair.zip (descriptionCounts, descs);
        val descriptionCountSum : counts = sumCounts descriptionCounts;*)

        val {words, longwords, vowels, sentences, ...} =
                addCounts (sentencesCountSum, descsSum)

        val paragraphCounts = {paragraphs = 1,
                               sentences = sentences,
                               words = words,
                               longwords = longwords,
                               vowels = vowels} : counts
    in
        ParagraphCount (paragraphCounts,
                        sentencesCountResults,
                        descsCountResults)
    end
  | countTextElement (Sentencifier.Heading content) =
    let
        val subCounts = map countTextElement content
        val total = sumCounts (map countOfTextCounts subCounts)
    in
        HeadingCount (total, subCounts)
    end
  | countTextElement (Sentencifier.Quotation content) =
    let
        val subCounts = map countTextElement content
        val total = sumCounts (map countOfTextCounts  subCounts)
    in
        QuotationCount (total, subCounts)
    end;
        

val countDocumentContent = map countTextElement;
    
(* Analysis methods *)
local
    open Real;
in
fun lix ({words, longwords, sentences, ...} : counts) = 
    let
        val r_words = fromInt words;
        val r_longwords = fromInt longwords;
        val r_sentences = fromInt sentences;
    in
        r_words/r_sentences + (r_longwords/r_words * 100.0)
    end;

fun fleshReadingEase ({words, sentences, vowels, ...} : counts) =
    let
        val r_words = fromInt words;
        (* Should be syllables, but that can be hard to count *)
        val r_vowels = fromInt vowels;
        val r_sentences = fromInt sentences;
    in
        206.835 - 1.015 * (r_words / r_sentences) - 84.6 * (r_vowels / r_words)
    end;

fun fleshKincaidGradeLevel ({words, sentences, vowels, ...} : counts) =
    let
        val r_words = fromInt words;
        (* Should be syllables, but that can be hard to count *)
        val r_vowels = fromInt vowels;
        val r_sentences = fromInt sentences;
    in
        0.39 * (r_words / r_sentences) + 11.8 * (r_vowels / r_words) - 15.59
    end;
end

fun checkSpelling languageCode str = 
    let open SpellChecker 
        open Option
        open Config in
        not (spell ()) orelse
        spellCheckWord (if isSome languageCode 
                        then valOf languageCode 
                        else Config.defaultLanguage ()) str
    handle dictionaryNotFound _ => true
         | badLanguageCode _ => true
         | badWord _ => true
    end;

local
    fun analyse' counts = {lix = lix counts,
                           fleshReadingEase = fleshReadingEase counts,
                           fleshKincaidGradeLevel = fleshKincaidGradeLevel counts}


    fun language attributes = List.find (fn Sentencifier.Language x => true
                                          | _ => false)
                                        attributes;

    fun spellCheckable attributes = List.exists (fn Sentencifier.Acronym => true
                                                  | _ => false)
                                        attributes;

    fun analyseSentenceElement doc_lang (Sentencifier.Word (t, attributes)) = 
        let
            val lang = case language attributes of
                           SOME (Sentencifier.Language x) => SOME x
                         | _ => doc_lang;
        in
            WordResult (t, not (spellCheckable attributes) orelse 
                           checkSpelling lang t)
        end
      | analyseSentenceElement _ (Sentencifier.Punctuation t) = PunctuationResult t;

    fun analyseSentence doc_lang (counts, sentence) =
            (analyse' counts,
             map (analyseSentenceElement doc_lang) sentence);

    fun resultOfTextResult (ParagraphResult (r, _, _)) = r
      | resultOfTextResult (HeadingResult (r, _)) = r
      | resultOfTextResult (QuotationResult (r, _)) = r;

    fun analyseTextElement doc_lang (ParagraphCount (count, sentenceCounts, descsCounts)) =
        let
            val total = analyse' count;
            val sentenceResults = map (analyseSentence doc_lang) sentenceCounts;
            val descriptionResults = map (map (analyseSentence doc_lang)) descsCounts;
        in
            ParagraphResult (total, sentenceResults, descriptionResults)
        end
      | analyseTextElement doc_lang (HeadingCount (count, subcounts)) =
            HeadingResult (analyse' count, map (analyseTextElement doc_lang) subcounts)
      | analyseTextElement doc_lang (QuotationCount (count, subcounts)) =
            QuotationResult (analyse' count, map (analyseTextElement doc_lang) subcounts)


    open List;
    fun isWord (Sentencifier.Word _) = true
      | isWord _ = false;

    fun sentenceHasWord sentence = exists isWord sentence;

    fun hasWord (Sentencifier.Paragraph (sentences, descs)) =
          (exists sentenceHasWord sentences) orelse
          (exists (exists sentenceHasWord) descs)
      | hasWord (Sentencifier.Heading sub) = exists hasWord sub
      | hasWord (Sentencifier.Quotation sub) = exists hasWord sub
in

fun analyse ({title, languagecode, content} : Sentencifier.document) =
    let
        val fcontent = filter hasWord content (* Remove paragraphs and more without words *)
        val documentCounts = countDocumentContent fcontent;
        val documentTotal = sumCounts (map countOfTextCounts documentCounts);
        val results = map (analyseTextElement languagecode) documentCounts;

        val titleResults = case title of
                               NONE => NONE
                             | SOME (sentences) =>
                               let
                                   val counts = map countSentence sentences;
                                   val zipped = ListPair.zip (counts, sentences);
                                   val totals = sumCounts counts;
                               in
                                   SOME (analyse' totals,
                                         map (analyseSentence languagecode) zipped)
                               end;
            
    in
        {titleResults = titleResults,
         documentResults = analyse' documentTotal,
         contentResults = results} : documentresult
    end;
end;

end;
