type nts = | Expr | Term | Binop | Num

let accept_all derivation fragment = Some (derivation, fragment)

let rec hasnums = function
	| [] -> false
	| (Num,_)::_ -> true
	| _::rest -> hasnums rest

let accept_nums derivation fragment = 
	if hasnums derivation then Some(derivation, fragment)
	else None

let gram =
  (Expr, function
     | Expr ->
	[[N Term; N Binop; N Expr];
	 [N Term]]
     | Term ->
	[[N Num];
	 [T"("; N Expr; T")"]]
     | Binop ->
	[[T"+"];
	 [T"-"];
	 [T"*"]]
     | Num ->
	[[T"1"]; [T"2"]; [T"3"];])


let test_1 = ((parse_prefix gram accept_all ["3";"*";"2";"+";"1"]) 
	= Some 
	([(Expr, [N Term; N Binop; N Expr]); 
	(Term, [N Num]); 
	(Num, [T "3"]);
   	(Binop, [T "*"]); 
	(Expr, [N Term; N Binop; N Expr]); 
	(Term, [N Num]);
   	(Num, [T "2"]); 
	(Binop, [T "+"]); 
	(Expr, [N Term]); 
	(Term, [N Num]);
   	(Num, [T "1"])], []))

(* Something is janky with test_2 and I'm not sure what :( *)

let test_2 = ((parse_prefix gram accept_nums ["3";"*";"2";"+";"1"]) <> None)
	&&	
	((parse_prefix gram accept_nums ["1"])
	= Some
	([(Expr, [N Term]);
	(Term, [N Num]);
   	(Num, [T "1"])], []))
	