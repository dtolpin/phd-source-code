(* IDA* search algorithm *)

type predictor = Fixed of float | Bounded of float

module type A(*lgorithm*) = sig
  type state
  type distance
  val shortest_path: 
    ?learn: ((*h1*)distance -> (*h2*)distance -> unit) ->
    ?worth: (state -> (*h*)distance -> (*L-g*)distance -> bool) ->
    (state, distance) Search.heuristic ->
    (state, distance) Search.heuristic ->
    state  -> (state, distance) Search.path

  val predictor: predictor ->
    (* learn, worth *)
    ((distance -> distance -> unit)*(state -> distance -> distance -> bool))
end 

module Make(P: Search.Problem) = struct
  type state = P.state
  type distance = P.distance

  open Stat

  let shortest_path ?(learn = fun _ _ -> ()) ?(worth = fun _ _ _ -> true)
      heur heur' state =
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
            let f =
              if worth state h (P.sub bound g) then
                let h' = ( inc_counter heur'_computed
                         ; call_timed (fun () -> heur' state)
                                      heur'_total_time )
                in ( learn h h' 
                   ; max f (P.add g h') )
              else f
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

    in call_timed
         (fun () ->
           ( reset_counters ()
           ; search state ))
         search_time

  let predictor pred =

    let make_worth p  = 
      fun state _ _ ->
        let tc =
          if !nodes_generated = 0 then 1.0 else
            !children_total_time/.float_of_int !nodes_generated in
        let th = 
          if !heur_computed = 0 then tc else
            !heur_total_time/.float_of_int !heur_computed in
        let th' = 
          if !heur'_computed = 0 then th else
            !heur'_total_time/.float_of_int !heur'_computed in
        let d = float_of_int (P.degree state - 1)  (* -1 for grandad *)
        in d > (th' -. p*.tc)/.((tc +. th +. th')*.p)

    in match pred with 

        Fixed p ->
          let learn _ _ = () in
          let worth = make_worth p in (learn, worth)

      | Bounded pmax -> 
          let n = ref 0 in      (* number of learning samples *)
          let sum = ref 0.0 in  (* sum of values of the random variable *)

          let learn h h' = 
            ( n := !n + 1
            ; if h' > h then
                sum := !sum +. 1.0 -. P.div h h' ) in 

          let worth state h left = 
            let p =
              let l = 1. -. (P.div h (max (P.add h P.one) left)) in
              let y = sqrt (2.*.float_of_int !n)*.l
              in if y < 1. then 1.0 (* not enough data *)
                else 
                  min 1.0 ((1. +. sqrt(log(y)))/.y
                           +. !sum/.float_of_int !n/.l)
            in (make_worth (min pmax p)) state h left

          in (learn, worth)
end

(* Derivation:

    (1-pH)*tH < pH*((d+1)*tC + d*th + (d-1)*tH) 
    tH < pH*d*tC + pH*tC + pH*d*th + pH*d*tH 
    d > (tH - pH*tC)/(pH*(tC+th+tH))
*)

