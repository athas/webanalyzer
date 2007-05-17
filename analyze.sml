(* 

1) Count number of words in the text (w)

2) Count number of periods in the text (p)

3) Count long words (7 or more chars) (lw)

4) Devide W with P. W/P = number of words pr period.
   Round to nerest integer or use 1 decimal.

5) Devide lw with w and multiply with 100 = prercentage of 
   long words in the text

6) The lix is calculated by ((w/p)+(lw/w x 100))

*)


(* 
 æ = 145 \221 , Æ = 146 \222
 ø = 248 \370 , Ø = 216 \330
 å = 134 \206 , Å = 143 \217 
 *)
val danish_vocals = [#"a", #"A", #"e", #"E", #"i", #"I", 
                     #"o", #"O", #"u", #"U", #"y", #"Y", 
                     chr(145), chr(222), chr(248), chr(216),
                     chr(134), chr(143)];

fun num_of_vocals(str, vocals: char list) = 
    let
        fun num_of_vocals'("", ret) = ret
          | num_of_vocals'(str, ret) = 
            (
             if (List.exists (fn x => x = String.sub(str, 0)) vocals ) then
                 num_of_vocals'(String.substring(str, 1, size str-1), ret +1)
             else
                 num_of_vocals'(String.substring(str, 1, size str-1), ret)    
            )
    in
        num_of_vocals'(str, 0)
    end;

(* return true if the char is a vocal acording to the vocal_list othervise false *)
fun is_vocal(chr, vocals: char list) = List.exists (fn x => x = chr) vocals


(* this function calculate number of words in the given text and number of
   long words >= 7
   
   RETURN (word, long_word, vocals)  *)
fun calc_words(s) =
    let

        (* if this is a long word then return 1 else 0 *)       
        add_lw(lw) = 
        (        
                if (String.size (List.hd s) >= 7) then
                  1
                else
                  0
        );

        (* w = words *)
        (* llw = lix long words *)
        (* v = vocals *)
        fun calc_words' ([], w, lw, v) = (w, lw, v)
          | calc_words' (s, w, lw, v) = 
            (
             
                (* NOTE we have chosen to ignore the possibility of 
                   periods in the middle of a word. There is no chance to 
                   guess what the author of the text ment by that. It could
                   be a typo but it could also be on pupose (as "f.eks.").
                   So as it ain't valid we chose no to act on it. *)

                (* Check for periods in the end of the text.
                   If the next word dosn't start with a capital letter then
                   we can be fairly sure that the period came from a 
                   abreviation or acronym, because we asume that the text is well written.  *)

                (* the actual period checking aint done here, because this 
                   function is ment to be used on 1 sentence at a time! *)

                calc_words'(List.tl s, 
                            w +1, 
                            lw +add_lw(List.hd s), 
                            v +num_of_vocals(List.hd s, danish_vocals);)
            );
            
    in
        (* tokenises the string to a string list acording to "isSpace" *)
        calc_words'(String.tokens Char.isSpace s, 0, 0, 0)

    end;


