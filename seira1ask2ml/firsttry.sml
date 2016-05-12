val rest = [42, 42, 42, 42, 42];
val m = 5;
val n = 3;

fun printlist n =
	if null n then (print("\n"))
	else (print(hd n); print(" "); printlist(tl n));

(* diavasma apo arxeio *)
fun readPoem(filename) =
    let val file = TextIO.openIn filename
        val poem = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n" ) poem
    end;

fun create_list n [] =
    if n = [] then []
    else if (tl n = []) then [Option.valOf(Int.fromString (hd n))]
    else (  [Option.valOf(Int.fromString(hd n))] @ create_list (tl n) [] );


fun diairese input =
    String.tokens ( fn c => c = #" " ) input;

fun diavasma filename =
    let val kati = readPoem filename in
    	 create_list (diairese(hd kati) @ diairese(hd (tl kati))) [] 
    end;

(* telos sinartisewn p diavazoun *)

(* vriskei teleutaio stoixeio listas *)
fun last n =
    if (tl n = []) then (hd n)
    else last (tl n);

(* vriskei to sinoliko athroisma tis listas *)
fun sum apot  lista =
    if (tl lista = []) then hd lista
    else (hd lista)  + sum apot (tl lista);


(* vriskei ektws apo ta M-N prwta ta ipoloipa athroismata *)
fun find_rest_sum rest lista =
    if ( rest = 1 ) then [sum 0 lista]
    else [hd lista] @ find_rest_sum (rest-1) (tl lista);

(* vriskei to megalitero *)
fun check_max max i =
    if (i > max) then i
    else max;


(* vriskei to megalitero tis listas *)
fun find_list_max l =
    if l=[] then 0
    else check_max (hd l) (find_list_max (tl l));


(* vriskei to elaxisto megalitero *)
(*fun find_least les na =
    let val megisto = find_list_max les
    	val mesos = (sum 0 les) div na in
	if (mesos < megisto) then megisto
	else mesos
	end;
*)
fun search lista panw katw sum =
    let val   orio = (panw+katw)div 2 in
    	if (tl lista = [] andalso sum+(hd lista) <= orio) then [Int.toString(hd lista)]
	else if (tl lista = [] andalso sum+(hd lista) > orio ) then ["|", Int.toString(hd lista)]
	else if (  sum+(hd lista) <= orio) then [Int.toString(hd lista)] @ search (tl lista) panw katw (sum+(hd lista))
	else ["|"] @ [Int.toString(hd lista)]  @ search (tl lista) panw katw (hd lista)
	end;

(*

*)
fun check_ele i k =
    if i="|" then (k+1)
    else k;

fun count_diaxwristes l =
    if l= [] then 1
    else check_ele (hd l) (count_diaxwristes (tl l));

fun add_sinola lista menoun =
    if (menoun = 0) then lista
    else [hd lista] @ ["|"] @ add_sinola (tl lista) (menoun-1);

fun psakse panw katw rest n =
    let val lista =  search rest panw katw 0 in
    	let val sinola = count_diaxwristes lista in
	    ((*print("panw="); print(Int.toString(panw));print(" katw="); print(Int.toString(katw)); print(" orio="); print(Int.toString((panw+katw)div 2));*)
	   if (sinola =n andalso panw = katw ) then printlist (List.rev(lista))
	   else if (panw = katw andalso sinola < n) then
	   (  	let val anapodi = List.rev(lista) in
		   printlist( add_sinola anapodi (n-sinola))
		    end)
	   else if (sinola > n andalso panw- katw =1) then
	   	   ((*print("sinola="); print(Int.toString(sinola)); print(" n="); print(Int.toString(n));*) psakse panw panw rest n)
	   else if (sinola < n andalso Int.abs(panw- katw) =1) then
	   ( let val anapodi = List.rev(lista) in
		   printlist( add_sinola anapodi (n-sinola))
		    end)
 	   else if (sinola = n andalso panw-katw = 1) then psakse katw katw rest n
	   else if (sinola <= n  ) then ((*print("edw"); print(Int.toString(katw));print("pali"); print(Int.toString(panw));*) psakse ((panw+katw) div 2) katw rest n)
	   else ((*print("KATI");print("sinola="); print(Int.toString(sinola)); print(" n="); print(Int.toString(n));*) psakse panw (((panw-katw)div 2) + katw) rest n))
	   end
	    end;

fun anazitisi m n rest =
    let val katw = find_list_max rest 
    	val panw = find_list_max (find_rest_sum (m-n) rest) in
	    psakse panw katw (List.rev(rest)) n
	    end;


fun program input =
    let val lista = (diavasma input) in
    	let val m = hd lista
	    val n = hd (tl lista)
	    val rest = tl(tl lista) in
	    	anazitisi m n rest
		(*rest*)
		end
	end
;

