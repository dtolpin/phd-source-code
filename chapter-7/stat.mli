(* algorithm statistics *)
val search_time: float ref
val nodes_expanded: int ref
val upg_nodes_expanded: int ref (* Lazy A* - the number of upgraded expanded nodes *)
val nodes_generated: int ref
val nodes_imexed: int ref
val children_total_time: float ref
val heur_computed: int ref
val heur_bypassed: int ref
val heur_total_time: float ref
val heur'_computed: int ref
val heur'_bypassed: int ref
val heur'_total_time: float ref
val number_of_iterations: int ref

val reset_counters: unit -> unit
val inc_counter: int ref -> unit
val call_timed: (unit -> 'a) -> float ref -> 'a

(* represent statistics as a string *)
val string_of_stat: unit -> string
