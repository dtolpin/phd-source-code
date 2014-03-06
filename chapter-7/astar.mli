(* A* search algorithm *)

module type A = sig
  type state
  type distance
  val shortest_path: 
    ?onInsert: (state -> unit) -> ?onExtract: (state -> unit) ->
    (state, distance) Search.heuristic -> state 
    -> (state, distance) Search.path
end 

module Make(P: Search.Problem): A
  with type state = P.state and type distance = P.distance
    
