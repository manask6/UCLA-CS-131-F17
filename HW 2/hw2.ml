type ('terminal, 'nonterminal) symbol =
    | T of 'terminal
    | N of 'nonterminal

(**********************************************************************************************************************)
(*** Convert Grammar function that takes a gram1 (Expr,rules_list) and converts to gram2 (Expr,production_function) ***)

let convert_grammar gram1 =

  let rec find_RHS nt rules_list = match rules_list with
	  [] -> []
	| (lhs, rhs)::tail ->
		if nt = lhs then rhs::(find_RHS nt tail)
		else find_RHS nt tail
  in
(fst gram1), fun nt -> (find_RHS nt (snd gram1));;

(**********************************************************************************************************************)



(*************************************************************************************************************************)
(*** Parse Prefix function that takes a gram and returns a matcher for gram ***)
 (*** Matcher takes an acceptor and fragment and returns what acceptor returns, or None if acceptor doesn't accept ***)
  (*** Sub-matcher matches the rule being tested against the fragment elements, returns what acceptor returns ***)

let parse_prefix gram accept frag =
  
  let rec matcher start_symbol rules_list alt_list accept derivation frag = match alt_list with
  	[] -> None
	| first_rule::rest_rules -> 
	    match (sub_matcher first_rule rules_list accept (derivation@[start_symbol, first_rule]) frag) with
		  None -> matcher start_symbol rules_list rest_rules accept derivation frag
		  | Some result -> Some result
  
  and sub_matcher test_rule rules_list accept derivation frag = match test_rule with
	[] -> accept derivation frag
	|_ -> match frag with
		[] -> None
		| prefix::suffix -> match test_rule with
			(T terminal)::rhs ->
				if prefix=terminal
				then (sub_matcher rhs rules_list accept derivation suffix)
				else None
			| (N nonterminal)::rhs -> 
				(matcher nonterminal rules_list (rules_list nonterminal) (sub_matcher rhs rules_list accept) derivation frag)

  in

matcher (fst gram) (snd gram) ((snd gram) (fst gram)) accept [] frag;;

(*************************************************************************************************************************)