(* f-lookahead heuristic - applies base heuristic
   to nodes upto the given f-depth *)

module type LA = sig
  type state
  type distance
  val distance_to_goal:
    (state, distance) Search.heuristic -> distance (* depth *) -> state
    -> distance
end

module Make(P: Search.Problem): LA
  with type state = P.state and type distance = P.distance
   

