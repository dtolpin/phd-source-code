(* Lazy A* search algorithm: accepts two heuristic
   functions and applies them lazily *)

module type A = sig
  type state
  type distance
  val shortest_path:
    ?worth: (state -> bool) -> (* aux. heuristic worth computing *)
    (state, distance) Search.heuristic -> 
    (state, distance) Search.heuristic -> 
    state -> (state, distance) Search.path

  val worth: state -> bool
end 

(* algorithm factory *)
module Make(P: Search.Problem): A
  with type state = P.state and type distance = P.distance
