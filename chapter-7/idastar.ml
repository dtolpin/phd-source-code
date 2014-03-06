(* IDA* search algorithm *)

module type A = sig
  type state
  type distance
  val shortest_path:
    (state, distance) Search.heuristic -> state
    -> (state, distance) Search.path
end

module Make(P: Search.Problem) = struct
  type state = P.state
  type distance = P.distance

  open Stat

  let shortest_path heur state =
    let nextbound = ref P.infty in

    (* return true if the node is in the current candidate solution *)
    let visited child solution = 
      let rec loop solution size =
        size != !Options.idastar_min_loop_size
        && match solution with
            (c,_)::solution ->  c = child || loop solution (size + 1)
          | [] -> false
      in loop solution 0 in
          
    (* depth-first search until the bound *)
    let dfs state bound =
      (* check whether the node belongs to a solution within the bound *)
      let rec down solution state g f =
        let h = ( inc_counter heur_computed
                ; call_timed (fun () -> heur state)
                  heur_total_time ) in
        let f = max f (P.add g h)
        in if f > bound then
            ( if f < !nextbound then nextbound := f
            ; None )
          else 
            let solution = (state,g)::solution
            in if P.is_goal state then Some (List.rev solution)
              else right ( inc_counter nodes_expanded
                         ; call_timed (fun () -> P.children state)
                                      children_total_time )
                         solution state g f 

      (* go over the children *)
      and right children solution state g f =
        match children with
            [] -> None
          | (child,distance)::children ->
            if visited child solution then 
              right children solution state g f
            else 
              ( inc_counter nodes_generated
              ; match down solution child (P.add g distance) f with
                  Some _ as maybe_solution -> maybe_solution
                | None ->
                  right children solution state g f )
      in down [] state P.zero P.zero in

    let search state = 
      (* loop through iterations of IDA*, for increasing bounds *)
      let rec iteration bound =
        ( inc_counter number_of_iterations
        ; let solution = dfs state bound
          in match solution with
              Some solution -> solution
            | None -> 
              let bound = !nextbound 
              in ( nextbound := P.infty 
                 ; iteration bound ) )
      in iteration (heur state)
    
(* in progress *)          
    in call_timed
         (fun () ->
           ( reset_counters ()
           ; search state ))
         search_time
end
