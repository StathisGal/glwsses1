fun check_number_of_digits n s =
	if n= 0 then s
	else check_number_of_digits (n div 10) (s+1);
fun rem_digits n per = 
	if (per = 1) then (check_number_of_digits n 0) 
	else if (per = 2) then (check_number_of_digits n 0)
	else ((check_number_of_digits n 0)-1) ;
	

fun pow s n =
	if n = 0 then 1
	else s * pow s  (n-1);

fun checkfirstdigit n =
	if n = 0 then 0
	else n div(pow 10 ((check_number_of_digits n 0)-1));	

fun checkseconddigit n =
	if n = 0 then 0
	else (n div(pow 10 ((check_number_of_digits n 0)-2))) mod 10;

fun find_case n =
	if checkfirstdigit n = n mod 10 then 1
	else if (checkfirstdigit n)-1 = n mod 10 then 2
	else if (checkfirstdigit n)*10 + checkseconddigit n = (n mod 10)+10 then 3
	else if (checkfirstdigit n)*10 + (checkseconddigit n)-1 = (n mod 10)+10 then 4
	else (5) ;
(* gia kathe arithmo an einai zigos einai o misos t alliws pairnoume enan standar*)
fun find_par1 n per =
	if per=1 then
		if n mod 2 = 0 then (n div 2)
		else if n = 1 then 0
		else 2
	else if per=2 then
		if (n) mod 2=0 then (n div 2)
		else if n = 1 then 0
		else 2
	else if per=4 then
		if n mod 2 = 0 then ((n + 10) div 2)
		else 8 
	else 0;

(*per einai i periptwsi*)
fun find_par2 n per=
	if per=1 then
		if n mod 2 = 0 then (n div 2)
		else if n= 1 then 1
		else n-2
	else if per=2 then
		if (n) mod 2 = 0 then (n div 2)
		else if n = 1 then 1
		else  n-2
	else if per=4 then
		if (n mod 2= 0) then ((n+10)div 2)
		else n+2
	else 0;
fun find_rest n =
	if find_case n=1 then (n mod(pow 10 ((check_number_of_digits n 0)-1)) div 10)
	else if find_case n=2 then (n mod(pow 10 ((check_number_of_digits n 0)-1)) div 10)	
	else if find_case n=3 then (n mod(pow 10 ((check_number_of_digits n 0)-2)) div 10)
	else if find_case n=4 then
	( let val rest = (n mod 10)+10 in
			let val dig = check_number_of_digits n 0 in 
				(n - (rest *( pow 10 (dig-2)))) div 10 -1
			end 
		end )
else (0);

fun anadromiki n remaining =
        let val per = find_case n in
		( 	print(Int.toString(per)); print("  ");
		        print(Int.toString(remaining)); print("  "); 
     			print(Int.toString(find_rest n)); print("  ");
        		print(Int.toString(find_par2 (n mod 10) per)); print("\n");
			if remaining = 0 then ""
			else Int.toString(find_par1 (n mod 10) per) ^ (anadromiki (find_rest n) (remaining-2)) ^ Int.toString(find_par2(n mod 10) per)
	)
	end;
		
(*
fun revsum n =
        if find_case n = 5 then "0"
        else
	(let val remaining = rem_digits n (find_case n) in
		 anadromiki n remaining;


(* 
fun revsum n =
	if n = 0 then ""
	else if find_case n = 5 then "0"
	else
	( let val per= find_case n in (
	print(Int.toString(per)); print("  ");
	print(Int.toString(find_rest n)); print("  "); 
	print(Int.toString(n mod 10)); print("  ");
	print(Int.toString(find_par2 (n mod 10) per)); print("\n");
	 Int.toString(find_par1 (n mod 10) per)^(revsum (find_rest n)) ^ Int.toString(find_par2 (n mod 10) per))
	end	
);
*)
