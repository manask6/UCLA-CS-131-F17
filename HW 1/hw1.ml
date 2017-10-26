let rec subset a b = match a with
     [] -> true
     | first::rest -> 
          if List.exists (fun x -> x=first) b 
          then subset rest b else false;;


let equal_sets a b = subset a b && subset b a;;


let rec set_union a b = match a with 
     [] -> b
     | first::rest -> 
          if List.exists (fun x -> x=first) b 
               then set_union rest b 
               else set_union rest (first::b);;


let set_intersection a b = 
     List.find_all (fun x -> List.mem x a) b;;


let set_diff a b =
     List.find_all (fun x -> List.for_all (fun y -> y <> x) b) a;;


let rec computed_fixed_point eq f x =
     if eq(f x) x then x
     else computed_fixed_point eq f (f x);;


let rec computed_periodic_point eq f p x =
     let rec helper eq f p x fx = match p with
          0 -> if eq x fx then x
               else helper eq f p (f x) (f fx)
          |_ -> helper eq f (p-1) x (f fx) 
in helper eq f p x x;;


let rec while_away s p x = 
     if (p x) then x::while_away s p (s x)
     else [];;


let rec rle_decode lp = match lp with
     [] -> []
     | (reps, value)::rest ->
          if reps = 0 then rle_decode rest
          else value::rle_decode((reps-1,value)::rest);;




type ('nonterminal, 'terminal) symbol =
     | N of 'nonterminal
     | T of 'terminal

let is_rhs_equal (a, b) (c, d) = equal_sets b d;;

let is_terminal_symbol symbol terminal_list = match symbol with
     T _ -> true
     | N s -> List.mem s terminal_list;;

let rec check_rules rules terminal_list = match rules with
     [] -> true
     | first::rest -> 
          if is_terminal_symbol first terminal_list
          then check_rules rest terminal_list
          else false;;

let rec build_grammar grammar terminal_list = match grammar with
     [] -> terminal_list
     | (x,rule)::rest -> 
          if (check_rules rule terminal_list) && not (List.mem x terminal_list)
          then build_grammar rest (x::terminal_list)
          else (build_grammar rest terminal_list);;

let build_grammar_wrapper (original_list, terminal_list) =
     original_list, (build_grammar original_list terminal_list);;

let rec filter rules terminal_list new_rules = match rules with
     [] -> new_rules
     | (l,r)::rest ->
          if check_rules r terminal_list
          then filter rest terminal_list (new_rules@[(l,r)])
          else filter rest terminal_list new_rules;;

let filter_blind_alleys g =
     (fst g), (filter (snd g) 
     (snd (computed_fixed_point is_rhs_equal build_grammar_wrapper ((snd g), []))) []);;
