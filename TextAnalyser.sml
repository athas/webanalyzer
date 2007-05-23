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

type results = AnalysisResult list;

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
type documentresult = {title_results : (results * sentenceresult list) option,
                       document_results : results,
                       content_results : textresult list};

(* A record containing information about a document, paragraph or sentence.
    - paragraphs is 0 and sentences is 1 for a sentence
    - paragraphs is 1 for a paragraph
 *)
type counts = {paragraphs : int,
               sentences : int,
               words : int,
               (* words with more than 7 chars, for LIX calculation *)
               longwords : int,
               vowels : int};

datatype textcounts = ParagraphCount of counts *
                                        (counts * Sentencifier.sentence) list *
                                        (counts * Sentencifier.sentence list) list
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

val vowels = explode "aeiouyæøåAEIOUYÆØÅöÖäÄ";

fun isVowel x = x member vowels;

fun countVowels str = foldl (fn (char, b) => if isVowel char
                                             then 1 + b
                                             else b)
                            0 
                            (explode str);

fun isLongword str = size str >= 7;

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
    
fun countOfTextCounts (ParagraphCount (c, _, _)) = c
  | countOfTextCounts (HeadingCount (c, _)) = c
  | countOfTextCounts (QuotationCount (c, _)) = c;


fun countTextElement (Sentencifier.Paragraph (sentences, descs)) =
    let
        val sentencesCounts = map countSentence sentences;
        val sentencesCountResults = ListPair.zip (sentencesCounts, sentences);
        val sentencesCountSum = sumCounts sentencesCounts;

        val descriptionCounts : counts list = map (sumCounts o (map countSentence)) descs;
        val descriptionCountResults : (counts * Sentencifier.sentence list) list = ListPair.zip (descriptionCounts, descs);
        val descriptionCountSum : counts = sumCounts descriptionCounts;

        val {words, longwords, vowels, sentences, ...} =
                addCounts (sentencesCountSum, descriptionCountSum)

        val paragraphCounts = {paragraphs = 1,
                               sentences = sentences,
                               words = words,
                               longwords = longwords,
                               vowels = vowels} : counts
    in
        ParagraphCount (paragraphCounts,
                        sentencesCountResults,
                        descriptionCountResults)
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
        r_words/r_sentences + (r_longwords/r_words * 100.0) handle Div => ~1.0
    end;

fun fleshReadingEase ({words, sentences, vowels, ...} : counts) =
    let
        val r_words = fromInt words;
        (* Should be syllables, but that can be hard to count *)
        val r_vowels = fromInt vowels;
        val r_sentences = fromInt sentences;
    in
        206.835 - 1.015 * (r_words / r_sentences) - 84.6 * (r_vowels / r_words) handle Div => ~1.0
    end;

fun fleshKincaidGradeLevel ({words, sentences, vowels, ...} : counts) =
    let
        val r_words = fromInt words;
        (* Should be syllables, but that can be hard to count *)
        val r_vowels = fromInt vowels;
        val r_sentences = fromInt sentences;
    in
        0.39 * (r_words / r_sentences) + 11.8 * (r_vowels / r_words) - 15.59 handle Div => ~1.0
    end;
end

fun checkSpelling languagecode str = true;


local
    fun analyse' counts = [Lix (lix counts),
                           FleshReadingEase (fleshReadingEase counts),
                           FleshKincaidGradeLevel (fleshKincaidGradeLevel counts)];



    fun analyseSentenceElement (Sentencifier.Word (t, _)) = WordResult (t, checkSpelling NONE t)
      | analyseSentenceElement (Sentencifier.Punctuation t) = PunctuationResult t;

    fun analyseSentence (counts, sentence) = (analyse' counts,
                                              map analyseSentenceElement sentence);

    fun resultOfTextResult (ParagraphResult (r, _, _)) = r
      | resultOfTextResult (HeadingResult (r, _)) = r
      | resultOfTextResult (QuotationResult (r, _)) = r;

    fun analyseTextElement (ParagraphCount (count, sentenceCounts, descsCounts)) =
        let
            val total = analyse' count;
            val sentenceResults = map analyseSentence sentenceCounts;
            (* val descriptionResults = map (map analyseSentence) descsCounts; *)
        in
            ParagraphResult (total, sentenceResults, [])
        end
      | analyseTextElement (HeadingCount (count, subcounts)) =
            HeadingResult (analyse' count, map analyseTextElement subcounts)
      | analyseTextElement (QuotationCount (count, subcounts)) =
            QuotationResult (analyse' count, map analyseTextElement subcounts)
in

fun analyse ({title, languagecode, content} : Sentencifier.document) =
    let
        val documentCounts = countDocumentContent content;
        val documentTotal = sumCounts (map countOfTextCounts documentCounts);
        val results = map analyseTextElement documentCounts;

        val titleResults = case title of
                               NONE => NONE
                             | SOME (sentences) =>
                               let
                                   val counts = map countSentence sentences;
                                   val zipped = ListPair.zip (counts, sentences);
                                   val totals = sumCounts counts;
                               in
                                   SOME (analyse' totals,
                                         map analyseSentence zipped)
                               end;
            
    in
        {title_results = titleResults,
         document_results = analyse' documentTotal,
         content_results = results}
    end;
end;

(*(* dummy *)
fun analyse ({title, languagecode, content} : Sentencifier.document) =
    {title_results = NONE,
     document_results = [],
     content_results = []} : documentresult; *)
end;
