structure TextAnalyser :> TextAnalyser =
struct

open Util;
infix 0 member;

type text = Sentencifier.text;

(* Result of a single word,
   the first boolean indicates whether the word is spelled correctly
   or not.
   the second boolean indicates whether the word is a repetition of
   the preceding word. *)
datatype SentenceElementResult = WordResult of text * bool * bool
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
                    | QuotationResult of textresult list;
(* All results of a document *)
type documentresult = {titleResults : (results * sentenceresult list) option,
                       documentResults : results,
                       contentResults : textresult list};


fun getLix (results : results) = #lix results;
fun getFRE (results : results) = #fleshReadingEase results;
fun getFKGL (results : results) = #fleshKincaidGradeLevel results;
fun getBadnessFactor (results : results) =
            (getLix results + (100.0 - (getFRE results / 3.0))) / 3.0;

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
  | countOfTextCounts (QuotationCount (c, _)) = zeroCount;


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
    in
        QuotationCount (zeroCount, subCounts)
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

    fun spellCheckable attributes = not (List.exists
                                             (fn Sentencifier.Acronym => true
                                               | Sentencifier.Code => true
                                               | _ => false)
                                             attributes);

    fun analyseSentenceElement doc_lang
                               (SOME (Sentencifier.Word (prev, _)))
                               (Sentencifier.Word (current, attrs)) = 
        let
            val lang = case language attrs of
                           SOME (Sentencifier.Language x) => SOME x
                         | _ => doc_lang;

            val isRepetition = Config.findRepetitions () andalso
                               Util.strToLower prev = Util.strToLower current
        in
            WordResult (current,
                        not (spellCheckable attrs) orelse
                        checkSpelling lang current,
                        isRepetition)
        end
      | analyseSentenceElement _ _ (Sentencifier.Punctuation w) = PunctuationResult w
      | analyseSentenceElement doc_lang _ (Sentencifier.Word(w, attrs)) =
        let
            val lang = case language attrs of
                           SOME (Sentencifier.Language x) => SOME x
                         | _ => doc_lang;
        in
            WordResult (w,
                        not (spellCheckable attrs) orelse
                        checkSpelling lang w,
                        false)
        end

    fun analyseSentenceElem' doc_lang (sElem as (Sentencifier.Word _), (prev, accumu)) = 
            (SOME sElem, analyseSentenceElement doc_lang prev sElem :: accumu)
      | analyseSentenceElem' doc_lang (sElem, (prev, accumu)) = 
            (prev, analyseSentenceElement doc_lang prev sElem :: accumu)

    fun analyseSentence'' doc_lang sentence = rev (#2 (foldl
                                                           (analyseSentenceElem' doc_lang)
                                                           (NONE, [])
                                                           sentence))

    fun analyseSentence doc_lang (counts, sentence as (x :: xs)) =
            (analyse' counts,
             analyseSentence'' doc_lang sentence) : sentenceresult
      | analyseSentence _ (counts, []) = (analyse' counts, [])

    fun analyseTextElement doc_lang
                           (ParagraphCount (count, sentenceCounts, descsCounts)) =
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
            QuotationResult (map (analyseTextElement doc_lang) subcounts)


    open List;
    fun isWord (Sentencifier.Word _) = true
      | isWord _ = false;

    fun sentenceHasWord sentence = exists isWord sentence;

    fun hasWord (Sentencifier.Paragraph (sentences, descs)) =
          (exists sentenceHasWord sentences) orelse
          (exists (exists sentenceHasWord) descs)
      | hasWord (Sentencifier.Heading sub) = exists hasWord sub
      | hasWord (Sentencifier.Quotation sub) = exists hasWord sub

    fun isMisspelled (WordResult (_, s, _)) = s
      | isMisspelled _ = false

    fun sentenceHasMisspelling (_, sentence) = exists isMisspelled sentence;

    fun hasMisspelling  (ParagraphResult (_, sentences, descs)) =
        (exists sentenceHasMisspelling sentences) orelse
        (exists (exists sentenceHasMisspelling) descs)
      | hasMisspelling (HeadingResult (_,sub)) = exists hasMisspelling sub
      | hasMisspelling (QuotationResult sub) = exists hasMisspelling sub

    (* results -> bool 
       Returns true if the specified result-set is ok, and false
       if they are to to wrong to be usable. *)
    fun evaluateResult result = (getFRE result >= ~50.0 andalso
                                getFRE result < 200.0)
                                
    fun badResultFilter (x as ParagraphResult (result, _, _)) = evaluateResult result orelse hasMisspelling x
      | badResultFilter (x as HeadingResult (result,_)) = evaluateResult result orelse hasMisspelling x
      | badResultFilter (QuotationResult _) = true

in

fun analyse ({title, languagecode, content} : Sentencifier.document) =
    let
        (* Remove paragraphs, heading and quotations without words *)
        val filtered_content = filter hasWord content
        val documentCounts = countDocumentContent filtered_content;
        val documentTotal = sumCounts (map countOfTextCounts documentCounts);
        val results = map (analyseTextElement languagecode) documentCounts;
        (* Remove all content with unusable results *)
        val filtered_results = filter badResultFilter results
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
         contentResults = filtered_results} : documentresult
    end;
end;

end;
