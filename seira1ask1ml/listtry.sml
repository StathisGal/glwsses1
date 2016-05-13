val test = [1,2,3,4,5,6,7,8,9];
val case4= [1,3,2,2,2];
fun create_list n [] =
	if n = 0 then []
	else (create_list (n div 10) []) @ [n mod 10];

fun list_to_number n lista =
	if length lista=1 then hd lista
	else (n + hd lista)*10 + list_to_number n (tl lista);




fun printlist n =
	if null n then (print("\n"))
	else (print(Int.toString(hd n)); printlist(tl n));

fun item x n = 
	if n=0 then hd x
	else item (tl x) (n-1);

fun last x =
	if length (tl x)= 0 then hd x
	else last (tl x);


fun find_case n =
	if hd n = last n andalso length n > 3 then 1
	else if (hd n)-1 = last n then 2
	else if (hd n)*10 + (item n 1) = (last n)+10 then 3
	else if (hd n)*10 + (item n 1)-1 = (last n)+10 then 4
	else if (hd n = 1 andalso hd(tl n)=0 andalso last n = 9) then 5
(*	else if (List.length n= 3 andalso *)
	else 7; 
(*
fun find_case n cases =
	 if hd n = last n then cases @ [1]
         if (hd n)-1 = last n then cases @ [2]
         if (hd n)*10 + (item n 1) = (last n)+10 then cases @ [3]
         if (hd n)*10 + (item n 1)-1 = (last n)+10 then cases @ [4];
       
*)
fun find_par1 n = 
	if n mod 2 = 0 then n div 2
		else (n+1) div 2; 

fun find_par2 n = 
	if n > 10 then 
		if n mod 2 = 0 then n div 2
		else (n-1) div 2
	else if n mod 2 = 0 then n div 2
		else (n-1) div 2;

fun find_rest n =
	let val per = find_case n in
		if (per=1) then rev(tl(rev(tl n)))
		else if per =2 then rev(tl(rev(tl n)))
		else if per =5 then [1] @ tl(tl(rev(tl(rev n))))
		else if length n = 3 then []
		else
			 let val arx= (item n 0)*10 +(item n 1) in
			[arx- (last n)-10] @ rev(tl(tl(rev(tl(tl n))))) @ [(last n)-1]
			end
	end;

fun anadromiki n =
	if null n then (printlist n; print("adadwadaweia"); "")
	else if n = nil then (print("adeidwa"); "" )
	else if length n =1 then (Int.toString((hd n)div 2)) 
	else if find_case n = 1 then
		(printlist n; print("1"); Int.toString(find_par1 ((last n))) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n))))
	else if find_case n = 2 then
		(printlist n; print("2"); Int.toString(find_par1 ((last n))) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n))))
	else if find_case n = 3 then
		(printlist n; print("3"); Int.toString(find_par1 ((last n))) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n))))
	else if find_case n = 4 then 
	(printlist n;	print("   4" );Int.toString(find_par1 ((last n)+10)) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n)+10)))
	else if find_case n = 5 then Int.toString(find_par1 (last n)) ^ anadromiki (find_rest n) ^ Int.toString (find_par2((last n)))
	else if find_case n = 6 then "error"
	else (printlist n; "error")
;

fun readPoem(filename) =
    let val file = TextIO.openIn filename
        val poem = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") poem
    end

fun readtoInt n  =
        Option.valOf(Int.fromString(hd (readPoem n)));

fun revsum n = 
	let val lista = create_list (readtoInt n) [] in
	    let val result = anadromiki lista in
	 if String.isSubstring "error" result then "0"
	 else result
	 end
	end;


