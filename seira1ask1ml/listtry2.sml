(* diavasma apo arxeio *)

fun readPoem(filename) =
    let val file = TextIO.openIn filename
        val poem = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") poem
    end

fun char_seString n =
    if (tl n) = [] then [Char.toString(hd n)]
    else [Char.toString(hd n)] @ char_seString(tl n);
fun spase_string s =
    let val head = hd s in
    let val lista_char = explode head in
    	char_seString (lista_char)
	end
	end;

fun create_list n =
    if null n then []
    else [Option.valOf(Int.fromString(hd n))] @ create_list (tl n);

(* telos diavasmatos apo arxeio *)

(* dimiourgei tin lista alla telika den xreiazetai*)
(*fun  create_list n [] =
	if n = 0 then []
	else (create_list (n div 10) []) @ [n mod 10];
*)
(* emfanisi tis listas *)

fun printlist n =
	if null n then (print("\n"))
	else (print(Int.toString(hd n)); printlist(tl n));

(* mas deixnei to antikeimeno pou theloume *)
fun item x n = 
	if n=0 then hd x
	else item (tl x) (n-1);

(* teleutaio antikeimeno *)
fun last x =
	if length (tl x)= 0 then hd x
	else last (tl x);

fun prelast x =
    if length (tl(tl x))=0 then hd x
    else prelast (tl x);

fun find_case n =
   if length n > 2 then
      if ((hd n= 1) andalso (item n 1)= 0 andalso (last n = 0)) then 1
      else if ((hd n= 1) andalso (item n 1) = 0 andalso last n = 9) then 2
      else if (hd n)*10 + (item n 1) = (last n)+10 then 3
      else if (hd n)*10 + (item n 1)-1 = (last n)+10 then 4
      else if (hd n)= last n then 5
      else if (hd n)-1 = last n then 6
      else ~1
   else if length n = 2 then
       if ((hd n= 1) andalso (item n 1)= 0 andalso (last n = 0)) then 8
       else if (hd n) = last n then 5
       else if (hd n)-1 = last n then 6
       else if (hd n = 1 andalso (hd(tl n) mod 2 = 0)) then 9
       else ~1
   else
	if ((hd n) mod 2 = 0 andalso ((hd n) <> 0)) then 7
	else ~1
;
      
fun find_par1 n = 
	if n mod 2 = 0 then n div 2
		else (n+1) div 2; 

fun find_par2 n = 
	if n mod 2 = 0 then n div 2
	else (n-1) div 2;
        
fun find_rest n =
    let val per = find_case n in
    	if (per = 1) then rev(tl(rev(tl n)))
	else if (per = 2) then  [1] @ tl(tl(rev(tl(rev n))))
	else if (per = 3 andalso (length n)>3) then
	    let val arx= (item n 0)*10 +(item n 1) in
	    let val prwtos= arx- (last n)-10 in
	    	if (prwtos <> 0) then
		   [prwtos] @ rev(tl(tl(rev(tl(tl n))))) @ [(prelast n)-1]
		else rev(tl(tl(rev(tl(tl n))))) @ [(prelast n)-1]
			end
			end
	else if (per = 4 andalso (length n)>3) then
	    let val arx= (item n 0)*10 +(item n 1) in
		let val prwtos= arx- (last n)-10 in
	    	if (prwtos <> 0) then
		   [prwtos] @ rev(tl(tl(rev(tl(tl n))))) @ [(prelast n)-1]
		else rev(tl(tl(rev(tl(tl n))))) @ [(prelast n)-1]
			end
			end
 	else if (per = 3 orelse per=4 ) then []
	else if (per = 5) then rev(tl(rev(tl n)))
	else if (per = 7) then []
	else  rev(tl(rev(tl n)))
	end;

fun anadromiki n =
    if null n then ""
    else
	let val per= find_case n in
	if per = 1 then  ((*printlist n; print("1 ");*) Int.toString(5) ^ anadromiki (find_rest n) ^ Int.toString (5))
	else if per = 2 then ((*printlist n; print("2 ");*) Int.toString(find_par1(last n)) ^ anadromiki (find_rest n) ^ Int.toString (find_par2(last n)))
	else if per = 3 then ((*printlist n; print("3 ");*) Int.toString(find_par1 ((last n)+10)) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n)+1)))
	else if per = 4 then ((*printlist n; print("per=4 " );*) Int.toString(find_par1 ((last n)+10)) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n)+10)))
	else if per = 5 then ((*printlist n; print("5 ");*) Int.toString(find_par1 ((last n))) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n))))
	else if per = 6 then ((*printlist n; print("6 ");*) Int.toString(find_par1 ((last n))) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n))))
	else if per = 7 then ((*printlist n; print("7 ");*) Int.toString(find_par1 (last n)))
	else if per = 8 then ((*printlist n; print("8 ");*) "5")
	else if per = 9 then ((*printlist n; print("9 ");*) Int.toString(((hd n)*10+(hd(tl n)))div 2))
	else ((*print(Int.toString(per));*) "error")
    end;

fun revsum n = 
	let val string_lista = spase_string (readPoem n)  in
	    let val lista = create_list string_lista in
    	    let val result = anadromiki lista in
	 	    if String.isSubstring "error" result then "0"
	 	    else result
		    end
	 	end
	end;

