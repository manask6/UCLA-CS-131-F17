let my_subset_test0 = subset [] []
let my_subset_test1 = not (subset [1;2;3] [4;5;6;1;2])
let my_subset_test2 = subset [1;2;3] [11;1;12;2;13;3]


let my_equal_sets_test0 = equal_sets [1; 2; 3] [1; 2; 2; 3]
let my_equal_sets_test1 = not (equal_sets [4] [1;2;3])


let my_set_union_test0 = equal_sets (set_union [1;2;3;4;5] [6;7;8;9]) [1;2;3;4;5;6;7;8;9]
let my_set_union_test1 = equal_sets (set_union [0;0] [1;1;1]) [0;1]


let my_set_intersection_test0 = equal_sets (set_intersection [1;2;3] [4;5;6]) [] 
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3] [2;3;4;5]) [2;3]


let my_set_diff_test0 = equal_sets (set_diff [1;2;3] [1;1;2;2;3;3]) []
let my_set_diff_test1 = equal_sets (set_diff [6;7] [1;2;3;4]) [6;7]


let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x mod 6) 100 = 4
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x mod 3) 27 = 0


let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x mod 6) 1 100 = 4
let my_computed_periodic_point_test1 = computed_periodic_point (fun x y -> x <= y) (fun x -> x + 5) 4 10 = 10


let my_while_away_test0 = equal_sets (while_away ((+) 3) ((>) 10) 0) [0; 3; 6; 9]
let my_while_away_test1 = equal_sets (while_away ((+) 2) ((>) 8) 1) [1; 3; 5; 7]



let my_rle_decode_test0 = rle_decode [2,0; 1,6] = [0; 0; 6]
let my_rle_decode_test1 = rle_decode [0,0; 1,1; 2,2; 3,3] = [1;2;2;3;3;3]




type nonterminals =
  | A | B | C | D

let rules =
   [A, [T "a"];
    A, [T "c"; N B];
    B, [T "d"];
    C, [N D; T "b"];
    D, [N C; T "a"]]

let grammar = A, rules

let my_filter_blind_alleys_test0 = filter_blind_alleys grammar = 
	(A, [A, [T "a"];
     A, [T "c"; N B];
     B, [T "d"]])