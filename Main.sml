(* The main interface to the web-analyzer program. Call the main
function (for example, main ["http://www.diku.dk"]) for
functionality. *)

open List;
open Util;
open TextIO;
open Http;
open TextAnalyser;

exception FatalError of string;

local
    structure URIMap = BinaryMapFn (struct type ord_key = URI; 
                                    fun compare (x, y) = String.compare(stringFromURI x, stringFromURI y);
                                    end);
    val URICache : URI URIMap.map ref = ref URIMap.empty;
    (* Remove everything to the right of the last picket fence (#) in
    the argument. *)
    fun trimURI string = 
        let val index = getLastIndexOf (#"#", string)
        in String.substring (string, 0, index) end
in
fun makeURI (absoluteURI, path) = 
    let
        val trimmedPath = trimURI path;
        val simpleURI = if (String.isPrefix "http://" trimmedPath)
                        then buildSimpleURI (NONE, trimmedPath)
                        else buildSimpleURI (absoluteURI, trimmedPath)
    in
        case URIMap.find (!URICache, simpleURI) of
            SOME uri => uri
          | NONE => (URICache := URIMap.insert (!URICache, simpleURI, buildURI (absoluteURI, trimmedPath));
                     makeURI (absoluteURI, trimmedPath))
    end
end

fun findLinks absoluteURI htmlTree = 
    let
        open HTMLParser;
        fun valid "a" link = not (String.isPrefix "mailto:" link) andalso
                             not (String.isPrefix "javascript:" link)
          | valid tagname link = true

        fun checkAttribute tag attribute =
            case getAttribute attribute tag of
                SOME str => if valid (tagName tag) str then
                                (SOME (makeURI (absoluteURI, str))
                                 handle Error _ => NONE)
                            else NONE
              | NONE => NONE
                        
        fun maybeGetLink "a" tag = checkAttribute tag "href"
          | maybeGetLink "frame" tag = checkAttribute tag "src"
          | maybeGetLink "iframe" tag = checkAttribute tag "src"
          | maybeGetLink "img" tag = checkAttribute tag "longdesc"
          | maybeGetLink "ins" tag = checkAttribute tag "cite"
          | maybeGetLink "del" tag = checkAttribute tag "cite"
          | maybeGetLink _ _ = NONE
    in
        SOMEs (mapLinks (fn tag => maybeGetLink (tagName tag) tag) htmlTree)
    end

fun filterExitLinks localuri links = 
    filter (fn link => serverFromURI link = 
                       serverFromURI localuri)
           links;

val getAndParse = HTMLParser.parse o getURI;
val analyseHTML = analyse o 
                  Sentencifier.sentencify o 
                  TextExtractor.extractFromHTML o 
                  HTMLFilter.filterhtml ;

fun findStartURI uri = if Robots.isPathAllowed (pathFromURI uri)
                       then uri
                       else let val rootURI = makeURI (SOME uri, "/") in
                                if Robots.isPathAllowed (pathFromURI rootURI) then
                                    rootURI before
                                    print (stringFromURI uri) before
                                    print " er forbudt for crawlere, så " before
                                    print (stringFromURI rootURI) before
                                    print " bruges i stedet.\n"
                                else
                                    raise FatalError ("Både den leverede URI og serverens rod-URI er forbudt, så jeg giver op.")
                            end

val visitedPages : URI list ref = ref [];
val waitingVisits : (URI list * int) list ref = ref [];

fun shouldVisit uri depth = depth <= (Config.crawlDepthLimit ()) andalso
                            contentTypeFromURI uri = "text/html" andalso
                            not (exists (fn x => x = uri) (!visitedPages)) andalso
                            Robots.isPathAllowed (pathFromURI uri);
  
fun visit outputAnalysis uri depth = 
let
    fun continue () =
        if length (!waitingVisits) = 0
        then ()
        else let val (uris, depth) = hd (!waitingVisits)
             in case uris of 
                    [] => (waitingVisits := tl (!waitingVisits);
                           continue ())
                  | (uri::xs) => (waitingVisits := (tl uris, depth)
                                                   :: (tl (!waitingVisits));
                                  visit outputAnalysis (hd uris) depth)
             end;
in
    if shouldVisit uri depth then
        (print "Besøger ";
         print (stringFromURI uri);
         print "\n";
         flushOut stdOut;
        let val parseTree = (getAndParse uri);
            val linksFound = if (depth+1) <= (Config.crawlDepthLimit ())
                             then filterExitLinks uri (findLinks (SOME uri) parseTree)
                             else []
        in
            Util.wait (Config.crawlDelay ());
            visitedPages := uri :: !visitedPages;
            waitingVisits := !waitingVisits @ [(linksFound, depth+1)];
            outputAnalysis uri (analyseHTML parseTree);
            if length linksFound > 0
            then (print "Fandt ";
                  print (Int.toString (length linksFound));
                  print " links.\n")
            else ();
            flushOut stdOut;
            continue ()
        end handle Error (HTTP (code, _)) => continue ()
                 | Error (Socket s) => continue ()
                 | Error (General s) => (print s; print "\n"; raise Fail "General"))
    else continue ()
end;

fun filenameForAnalysis uri = (String.map (fn #"/" => #"#"
                                           | char => char)
                                         ((serverFromURI uri) ^ (pathFromURI uri)))
                              ^ ".html";

fun writeIndex starturi outputDir outputFilename analysedPages =
    let open HTMLBuilder;
        val sortedResults = ListMergeSort.sort (fn ((_, x), (_, y)) => 
                                                   getBadnessFactor (documentResults y) >
                                                   getBadnessFactor (documentResults x))
                                               analysedPages
        infix &&;
        val std = td o $
        val wseqFromURI = $ o stringFromURI
        val wltable = table o $$ o (List.map flatten)
        val pagetitle = "Analyse af " ^ (stringFromURI starturi)
    in writeTo (OS.Path.concat(outputDir, "index.html"))
               (flatten
                    (html ((head o title o $) pagetitle) &&
                          (body ((h1 ($ pagetitle)) &&
                                      (h3 ($ "V&aelig;lg en underside for detaljer")) &&
                                      (wltable ((List.map
                                                     (fn (uri, result) =>
                                                         let val style = ("style=\"color:black; background-color: "
                                                                          ^ (TextAnalysisReporter.colorByResults
                                                                                 (TextAnalyser.documentResults result))
                                                                          ^ "\"");
                                                         in
                                                             (tr (tda style
                                                                      (ahrefa (urlencode (outputFilename uri)) style
                                                                              (wseqFromURI uri))))
                                                         end)
                                                     sortedResults)))))))
    end;
   
fun mainProgram (arg :: rest) = 
    let 
        val uri = makeURI (NONE, arg)
            handle Error (Socket s) => raise FatalError s
        val robotsuri = makeURI (NONE, protocolFromURI uri
                                       ^ "://"
                                       ^ serverFromURI uri
                                       ^ "/robots.txt")
        val robotstxt = (getURI robotsuri)
            handle Error (HTTP (_, _)) => ""
                 | Error (Socket s) => ""
        val _ = Robots.initRobotsTxt robotstxt;
        val starturi = findStartURI uri
        val outputDir = if isSome (Config.outputDir ()) 
                        then valOf (Config.outputDir ())
                        else serverFromURI uri

        fun outputFilename uri = filenameForAnalysis uri
        val analysedPages : (URI * documentresult) list ref = ref []
        fun analysisOutputter uri analysis =
            (writeTo (OS.Path.concat(outputDir, outputFilename uri)) 
                     (TextAnalysisReporter.makeReport analysis)
            before analysedPages := (uri, analysis) :: !analysedPages);
    in 
        if (SpellChecker.spellCheckingAvailable ())
        then print ("Stavekontrol er tilgængelig.\n")
        else (print ("Stavekontrol er ikke tilgængelig.\n");
              if (Config.spell ())
              then Config.toggleSpell ()
              else ());
        OS.FileSys.mkDir outputDir 
        handle OS.SysErr (_,_) => raise FatalError ("Kunne ikke oprette output-mappe \"" ^ outputDir ^ "\".");
        (* Useful when used interactively. *)
        visitedPages := [];
        waitingVisits := [];
        visit analysisOutputter starturi 0;
        writeIndex starturi outputDir outputFilename (!analysedPages);
        print "Analysen er færdig! Åben ";
        print outputDir;
        print "/index.html i en internetbrowser, for at aflæse resultatet. \n";
        flushOut stdOut;
        OS.Process.success
    end
  | mainProgram [] = raise FatalError "Ikke nok argumenter." before Help.printProgramArgs();

(*
 Arguments to be added.

*)


fun parseArguments ("-d" :: limit :: rest) =
       (Config.setCrawlDepthLimit o valOf o Int.fromString) limit before
       parseArguments rest
  | parseArguments ("-u" :: userAgent :: rest) = 
       Config.setHttpUserAgent userAgent before parseArguments rest
  | parseArguments ("-c" :: delay :: rest ) =
       (Config.setCrawlDelay o valOf o Int.fromString) delay before parseArguments rest
  | parseArguments ("-lix" :: rest) =
       Config.toggleLix () before parseArguments rest
  | parseArguments ("-fre" :: rest) = 
       Config.toggleFre () before parseArguments rest
  | parseArguments ("-fkgl" :: rest) =
       Config.toggleFkgl () before parseArguments rest
  | parseArguments ("-spell" :: rest) =
       Config.toggleSpell () before parseArguments rest
  | parseArguments ("-wordrep" :: rest) =
       Config.toggleFindRepetitions () before parseArguments rest
  | parseArguments ("-l" :: language :: rest) =
       Config.setDefaultLanguage language before parseArguments rest
  | parseArguments ("-o" :: outputDir :: rest) = 
       Config.setOutputDir outputDir before parseArguments rest
  | parseArguments ("-ignore-tag" :: tagname :: rest) = 
       Config.addTagNameFilter tagname before parseArguments rest
  | parseArguments ("-ignore-id" :: id :: rest) = 
       Config.addIdFilter id before parseArguments rest
  | parseArguments (_ :: rest) = parseArguments rest
  | parseArguments [] = ();

(* Analyze the website accessible at the URL provided by the first
element of the args list. Will create a file "[domain-name].html" in
the current directory containing links to HTML-files inside a folder
"[domain-name]" that contains the actual analysis results. *)
fun main (name, args) = (Config.setDefaults; parseArguments args; mainProgram args)
    handle FatalError reason => OS.Process.failure before print "Fejl: " before print reason before print "\n";

(*val _ = mainWrapper (CommandLine.arguments ());*)
