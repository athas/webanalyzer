(* The main interface to the web-analyzer program. Call the main
function (for example, main ["http://www.diku.dk"]) for
functionality. *)

open TextIO;
open Http;
open HTMLParser;
open List;
open Util;

exception FatalError of string;

fun mapLinks function htmlTree =
    let
        (* takes a parsetree list and processes down it by calling
           mapLinks' on the children of the tree *)
        fun getChildren' (ret, []) = ret
          | getChildren' (ret, (child :: rest)) =
            getLinks' (ret, child) @ getChildren' ([], rest)
            
        (* Checks each parsetree for Tag's and if the tags are what
           we looks for then it scans the tags known link attributes
           and adds then return all these links. 
           'AND' because these to functions are mutually recursive *)
        and getLinks' (ret, Text t) = ret
          | getLinks' (ret, Tag (tag, [])) = ret
          | getLinks' (ret, Tag (tag, children)) =     
            case tagName tag of
                "a" => function tag :: getChildren' (ret, children)
              | "frame" => function tag :: getChildren' (ret, children)
              | "iframe" => function tag :: getChildren' (ret, children)
              (* If none of the above cases just move on to next tag in the parsetree *)
              | _ => getChildren' (ret, children)
    in
        getChildren'([], htmlTree)
    end

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
        fun valid "a" link = not (String.isPrefix "mailto:" link)
          | valid tagname link = true

        fun checkAttribute tag attribute =
            case getAttribute attribute tag of
                SOME str => if valid (tagName tag) str then
                                (SOME (makeURI (absoluteURI, str))
                                 handle Http.Error (General _) => NONE)
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

val getAndParse = parse o getURI;
val analyseHTML = TextAnalyser.analyse o 
                  Sentencifier.sentencify o 
                  TextExtractor.extractFromHTML;

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
val waitingVisits : (unit -> unit) list ref = ref [];

fun shouldVisit uri depth = depth < (Config.crawlDepthLimit ()) andalso
                            contentTypeFromURI uri = "text/html" andalso
                            not (exists (fn x => x = uri) (!visitedPages)) andalso
                            Robots.isPathAllowed (pathFromURI uri);

fun visit outputAnalysis depth uri = 
let
    fun continue () = if length (!waitingVisits) = 0
                      then ()
                      else let val next = hd (!waitingVisits)
                           in waitingVisits := tl (!waitingVisits);
                              next ()
                           end
in
    if shouldVisit uri depth then
        let val parseTree = (getAndParse uri);
            val linksFound = filterExitLinks uri (findLinks (SOME uri) parseTree);
            fun visitChilds () = app (visit outputAnalysis (depth+1)) linksFound;
        in
            Util.wait (Config.crawlDelay ());
            visitedPages := uri :: !visitedPages;
            waitingVisits := !waitingVisits @ [visitChilds];
            print "Visiting ";
            print (stringFromURI uri);
            print (" at depth " ^ (Int.toString depth) ^ "\n");
            flushOut stdOut;
            outputAnalysis uri (analyseHTML parseTree);
            map (fn link => (print "Seeing ";
                             print (stringFromURI link);
                             print (" at depth " ^ (Int.toString (depth+1)) ^ "\n");
                             flushOut stdOut))
                linksFound;
            continue ()
        end handle Error (HTTP (code, _)) => continue ()
                 | Error (Socket s) => continue ()
                 | Error (General s) => (print s; print "\n"; raise Fail "General")
    else ()
end;
fun filenameForAnalysis uri = String.map (fn #"/" => #"#"
                                           | char => char)
                                         ((serverFromURI uri) ^ (pathFromURI uri));

fun badnessFactor analysis = case List.find (fn (TextAnalyser.Lix value) => true
                                                         | _ => false)
                                            (TextAnalyser.documentResults analysis)
                              of SOME (TextAnalyser.Lix value) => value
                               | _ => raise Fail "Impossible! No lix!";

fun writeIndex starturi outputFilename analysedPages =
    let open HTMLBuilder;
        val sortedResults = ListMergeSort.sort (fn ((_, x), (_, y)) => badnessFactor y > badnessFactor x) analysedPages
        val std = td o $
        val tdr = tda "align=\"right\""
        val wseqFromURI = $ o stringFromURI
        val wseqFromReal = $ o Util.formatForOutput
        val wltr = tr o $$ o (List.map flatten)
        val wltable = table o $$ o (List.map flatten)
    in writeTo (serverFromURI starturi ^ ".html")
               (flatten
                    (html (&& ((head o title o $) ("Analyse af " ^ (stringFromURI starturi)),
                               (body (wltable ((wltr [(std "Sidesv&aelig;rhedsgrad"), (std "URI")]) ::
                                               (List.map
                                                    (fn (uri, result) =>
                                                        (wltr [(tdr (wseqFromReal (badnessFactor result))),
                                                               (td (ahref (urlencode (outputFilename uri))
                                                                          (wseqFromURI uri)))]))
                                                    sortedResults))))))))
    end;
    
fun mainProgram (arg :: rest) = 
    let 
        val uri = makeURI (NONE, arg)
        val robotsuri = makeURI (NONE, protocolFromURI uri
                                       ^ "://"
                                       ^ serverFromURI uri
                                       ^ "/robots.txt")
        val robotstxt = (getURI robotsuri)
            handle Error (HTTP (_, _)) => ""
                 | Error (Socket s) => ""
        val _ = Robots.initRobotsTxt robotstxt;
        val starturi = findStartURI uri
        val outputdir = serverFromURI uri;
        fun outputFilename uri = (outputdir ^ "/" ^ (filenameForAnalysis uri))
        val analysedPages : (URI * TextAnalyser.documentresult) list ref = ref []
        fun analysisOutputter uri analysis =
            (writeTo (outputFilename uri) (TextAnalysisReporter.makeReport analysis)
            before analysedPages := (uri, analysis) :: !analysedPages);
    in 
        OS.FileSys.mkDir outputdir
        handle OS.SysErr (_,_) => raise FatalError "Kunne ikke oprette output-mappe.";
        (* Useful when used interactively. *)
        visitedPages := [];
        visit analysisOutputter 0 starturi;
        writeIndex starturi outputFilename (!analysedPages);
        print "Done!\n";
        flushOut stdOut
    end
  | mainProgram [] = raise FatalError "Ikke nok argumenter." before Help.printProgramArgs();

fun parseArguments ("-d" :: limit :: rest) =
       (Config.setCrawlDepthLimit o valOf o Int.fromString) limit before
       parseArguments rest
  | parseArguments ("-u" :: userAgent :: rest) = Config.setHttpUserAgent userAgent before
                                                 parseArguments rest
  | parseArguments ("-c" :: delay :: rest ) =
       (Config.setCrawlDelay o valOf o Int.fromString) delay before
       parseArguments rest
  | parseArguments ("-lix" :: rest) =
       Config.setLix () before parseArguments rest
  | parseArguments ("-fre" :: rest) = 
       Config.setFre () before parseArguments rest
  | parseArguments ("-fkgl" :: rest) =
       Config.setFkgl () before parseArguments rest
  | parseArguments ("-spell" :: rest) =
       Config.setSpell () before parseArguments rest
  | parseArguments ("-l" :: language :: rest) =
    Config.setDefaultLanguage (language) before parseArguments rest
  | parseArguments (_ :: rest) = parseArguments rest
  | parseArguments [] = ();

(* Analyze the website accessible at the URL provided by the first
element of the args list. Will create a file "[domain-name].html" in
the current directory containing links to HTML-files inside a folder
"[domain-name]" that contains the actual analysis results. *)
fun main args = Config.setDefaults() before parseArguments args before mainProgram args
    handle FatalError reason => print "Fejl: " before print reason before print "\n";

(*val _ = mainWrapper (CommandLine.arguments ());*)
