val test = [1,2,3,4,5,6,7,8,9];
val case4= [1,3,2,2,2];

fun readPoem(filename) =
    let val file = TextIO.openIn filename
        val poem = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") poem
    end

fun readtoInt n  =
        Option.valOf(Int.fromString(hd (readPoem n)));

fun reverse s =Option.valOf(Int.fromString (implode (foldl op:: [] (explode (Int.toString(s))))));
;


fun find_num n target =
    if (n + reverse n = target ) then n
    else if n = 0 then n
    else find_num (n-1) target
;




fun revsum input =
    let val target = readtoInt (input) in
    	find_num (target div 2) target
	end;

fun for n lim =
    if (n > lim) then (print "\ntelos\n")
    else (
    	 let val result= find_num (n div 2) n in
    	 if ( result= 0) then (for (n+1) lim)
	 else (print(Int.toString(n)); print("\t");  for (n+1) lim)
	 end) ;

