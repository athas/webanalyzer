signature HTMLParser =
sig
    
    val parse : HTMLLexeme.lexeme list -> HTMLParseTree.parsetree list;

end
