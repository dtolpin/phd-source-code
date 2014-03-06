(* Lazy IDA* search algorithm: accepts two heuristic
   functions and applies them lazily *)

type predictor = Fixed of float | Bounded of float

module type A(*lgorithm*) = sig
  type state
  type distance
  val shortest_path: 
    ?learn: ((*h1*)distance -> (*h2*)distance -> unit) ->
    ?worth: (state -> (*h1*)distance -> (*L-g*)distance -> bool) ->
    (state, distance) Search.heuristic ->
    (state, distance) Search.heuristic ->
    state  -> (state, distance) Search.path

  val predictor: predictor ->
    (* learn, worth *)
    ((distance -> distance -> unit)*(state -> distance -> distance -> bool))
end

module Make(P: Search.Problem): A
  with type state = P.state and type distance = P.distance
