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
    val URICache = ref (Binarymap.mkDict (fn (x, y) => String.compare (stringFromURI x, stringFromURI y)))
                   
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
        case Binarymap.peek (!URICache, simpleURI) of
            SOME uri => uri
          | NONE => (URICache := Binarymap.insert (!URICache, simpleURI, buildURI (absoluteURI, trimmedPath));
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
                  Sentencifier.sentencifyParagraphised o 
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

fun visit outputAnalysis uri depth = 
    if depth >= (Config.crawlDepthLimit ()) orelse 
       contentTypeFromURI uri <> "text/html" orelse
       exists (fn x => x = uri) (!visitedPages) 
    then ()
    else let val parseTree = (getAndParse uri) in
             Util.wait (Robots.getCrawlDelay ());
             visitedPages := uri :: !visitedPages;
             print "Visiting ";
             print (stringFromURI uri);
             print "\n";
             flushOut stdOut;
             outputAnalysis uri (analyseHTML parseTree);
             map (fn link => (print "Seeing ";
                              print (stringFromURI link);
                              print "\n";
                              flushOut stdOut;
                              visit outputAnalysis link (depth + 1)))
                 (filterExitLinks uri (findLinks (SOME uri) parseTree));
             ()
         end
         handle Error (HTTP (code, _)) => ()
              | Error (Socket s) => ()
              | Error (General s) => (print s; print "\n"; raise Fail "General")

fun filenameForAnalysis uri = String.map (fn #"/" => #"#"
                                           | char => char)
                                         ((serverFromURI uri) ^ (pathFromURI uri));

fun badnessFactor analysis = case List.find (fn (TextAnalyser.Lix value) => true
                                                         | _ => false)
                                            (TextAnalyser.documentResults analysis)
                              of SOME (TextAnalyser.Lix value) => value
                               | _ => raise Fail "Impossible! No lix!";

(* Compare two analysis-results. *)
fun compareResults (result1, result2) = Real.compare (badnessFactor result1, badnessFactor result2);

fun writeIndex starturi outputFilename analysedPages =
    let open Msp;
        val sortedResults = Listsort.sort (fn ((_, x), (_, y)) => compareResults (y, x)) analysedPages
        val std = td o $
        val wseqFromURI = $ o stringFromURI
        val wseqFromReal = $ o Real.toString
        val wltr = tr o $$ o (List.map flatten)
        val wltable = table o $$ o (List.map flatten)
    in writeTo (serverFromURI starturi ^ ".html")
               (flatten (html (&& ((head o title o $) ("Analyse af " ^ (stringFromURI starturi)),
                                   (body (wltable ((wltr [(std "URI"), (std "Sidesværhedsgrad")]) ::
                                                   (List.map (fn (uri, result) =>
                                                                 (wltr [(td (ahref (urlencode (outputFilename uri))
                                                                                   (wseqFromURI uri))),
                                                                        (td (wseqFromReal (badnessFactor result)))]))
                                                             sortedResults))))))))
    end;

fun mainProgram (arg :: rest) = 
    let val uri = makeURI (NONE, arg)
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
            writeTo (outputFilename uri) (TextAnalysisReporter.makeReport analysis)
            before analysedPages := (uri, analysis) :: !analysedPages
    in 
        FileSys.mkDir outputdir;
        (* Useful when used interactively. *)
        visitedPages := [];
        visit analysisOutputter starturi 0;
        writeIndex starturi outputFilename (!analysedPages);
        print "Done!\n";
        flushOut stdOut
    end
  | mainProgram [] = print "Not enough arguments\n";

fun parseArguments ("-d" :: limit :: rest) =
    (Config.setCrawlDepthLimit o valOf o Int.fromString) limit before
    parseArguments rest
  | parseArguments (_ :: rest) = parseArguments rest
  | parseArguments [] = ();

fun main args = parseArguments args before mainProgram args
    handle FatalError reason => print reason before print "\n";

(*val _ = mainWrapper (CommandLine.arguments ());*)
