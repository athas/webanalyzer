signature deHTMLifier =
sig
    type text = string;

           
    datatype textelement = RegularText of text
                         | Paragraph of textelement list
                         | Heading of textelement list
                         (*  | Table of *)
                         | Link of textelement list
                         | Quotation of textelement list
                         | Description of text (* for alt, title and summary attributes *)
                         | Code of text (* var, code, kbd *)

    type document = {title : string option,
                     languagecode : string option,
                     content : textelement list};

    val dehtmlify : HTMLParser.parsetree list -> document;

end
