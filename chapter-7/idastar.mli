(* IDA* search algorithm *)

module type A(*lgorithm*) = sig
  type state
  type distance
  val shortest_path: 
    (state, distance) Search.heuristic -> state 
    -> (state, distance) Search.path
end 

module Make(P: Search.Problem): A
  with type state = P.state and type distance = P.distance
