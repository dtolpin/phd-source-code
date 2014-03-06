(* constants *)
val astar_openlist_initial_size: int
val astar_closedlist_initial_size: int

(* runtime options *)
val astar_expand_immediately: bool ref
val lazya_heuristic_bypassing: bool ref

(* tuning parameters, can be left untouched *)
val lazya_heur'_computed_prior: int ref
val lazya_heur'_helpful_prior: int ref

val idastar_min_loop_size: int ref
