

(* Basic IO stuff *)
exception IOError of string;

fun readFrom fileName = 
    let
      val inStrm = (TextIO.openIn fileName) handle _ => raise IOError fileName
    in
        TextIO.inputAll inStrm
    end


(* Some usefull string functions *)

(* returns the index of the first occurence of chr in str *)
fun getFirstIndexOf (chr, str) = 
    let
        fun getFirstIndexOf' chr index "" = index
          | getFirstIndexOf' chr index str =
            if (String.sub (str, index) = chr) then
                index
            else
                getFirstIndexOf' chr (index+1) str
                
    in
        getFirstIndexOf' chr 0 str 
    end

(* trims a string from spaces in front and back *)
fun trimStr str = hd (String.tokens Char.isSpace str)
