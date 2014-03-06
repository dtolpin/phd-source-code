(* f-lookahead heuristic *)
module type LA = sig
  type state
  type distance
  val distance_to_goal:
    (state, distance) Search.heuristic -> distance (* depth *) -> state
    -> distance
end

module Make(P: Search.Problem) = struct
  type state = P.state
  type distance = P.distance

  let distance_to_goal heur depth state =
    let boundary = P.add depth (heur state) in
    let rec dfs state g =
      if P.is_goal state then g
      else
        let f = P.add g (heur state)
        in if f >= boundary then f
           else
             List.fold_left min P.infty
               (List.map
                  (fun (sc, dc) -> dfs sc (P.add g dc))
                  (P.children state))
    in dfs state P.zero
end
