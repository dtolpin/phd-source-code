let astar_openlist_initial_size = 2976221 (* Mersenne prime number, borrowed *)
let astar_closedlist_initial_size = 6972593 (* Mersenne prime number, borrowed *)

let astar_expand_immediately = ref true
let lazya_heuristic_bypassing = ref false

let lazya_heur'_computed_prior = ref 1000
let lazya_heur'_helpful_prior = ref 500

let idastar_min_loop_size = ref 3

