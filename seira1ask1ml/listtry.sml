val test = [1,2,3,4,5,6,7,8,9];
val case4= [1,3,2,2,2];
fun create_list n [] =
	if n = 0 then []
	else (create_list (n div 10) []) @ [n mod 10];


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
	else 5; 
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
		else if length n = 3 then []
		else
			 let val arx= (item n 0)*10 +(item n 1) in
			[arx- (last n)-10] @ rev(tl(tl(rev(tl(tl n))))) @ [(last n)-1]
			end
	end;

fun anadromiki n =
	if null n then (printlist n; print("adeia"); "")
	else if n = nil then (print("adeia"); "" )
	else if length n =1 then (Int.toString((hd n)div 2)) 
	else if find_case n = 1 then
		(printlist n; print("1"); Int.toString(find_par1 ((last n))) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n))))
	else if find_case n = 2 then
		(printlist n; print("2"); Int.toString(find_par1 ((last n))) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n))))
	else if find_case n = 3 then
		(printlist n; print("3"); Int.toString(find_par1 ((last n))) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n))))
	else if find_case n = 4 then 
	(printlist n;	print("   4" );Int.toString(find_par1 ((last n)+10)) ^ anadromiki (find_rest n) ^ Int.toString(find_par2((last n)+10)))
	else (printlist n; "error")
;
fun revsum n = 
	let val lista = create_list n [] in
	if find_case lista= 5 then "0"
	else if String.isSubstring "error" (anadromiki lista) then "0"
	else anadromiki lista
	end;


