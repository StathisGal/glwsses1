fun check_number_of_digits n s =
	if n= 0 then s
	else check_number_of_digits (n div 10) (s+1);

fun pow s n =
	if n = 0 then 1
	else s * pow s  (n-1);

fun checkfirstdigit n =
	n div(pow 10 ((check_number_of_digits n 0)-1));	

fun checkseconddigit n =
	(n div(pow 10 ((check_number_of_digits n 0)-2))) mod 10;

fun find_case n =
	if checkfirstdigit n = n mod 10 then 1
	else if (checkfirstdigit n)-1 = n mod 10 then 2
	else if (checkfirstdigit n)*10 + checkseconddigit n = (n mod 10)+10 then 3
	else if (checkfirstdigit n)*10 + (checkseconddigit n)-1 = (n mod 10)+10 then 4
	else (5) ;

fun find_par1 n =
	if n mod 2 = 0 then (n div 2)
	else 2;
fun find_par2 n =
	if n mod 2 = 0 then (n div 2)
	else n-2;

fun find_rest n =
	if find_case n=1 then (n mod(pow 10 ((check_number_of_digits n 0)-1)) div 10)
	else (0-1);

fun revsum n =
	if n = 0 then ""
	else if find_case n = 5 then "0"
	else (print("coming soon\n"); Int.toString(find_par1 (checkfirstdigit n))^(revsum (find_rest n)) ^ Int.toString(find_par2 (checkfirstdigit n)));

