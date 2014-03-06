(* A* search algorithm *)

module type A = sig
  type state
  type distance
  val shortest_path:
    ?onInsert: (state -> unit) -> ?onExtract: (state -> unit) ->
    (state, distance) Search.heuristic -> state
    -> (state, distance) Search.path
end

module Make(P: Search.Problem) = struct
  type state = P.state
  type distance = P.distance

  open Stat

  (* collections for open and closed `lists' *)
  type node = {n_f: distance; n_g: distance;
               n_parent: P.pstate option}
  module Nodetab = Hashtbl(*.Make(P) (* of nodes *)*)
  module OpenList = Base.PriorityQueue(*.Make(P) (* shortest first *)*)

  let shortest_path ?(onInsert=ignore) ?(onExtract=ignore) heur state =
    let nodetab = Nodetab.create Options.astar_closedlist_initial_size in

    (* ord a b = true means that the priority of b is not higher
       than the priority of a *)
    let order psa psb =
      let {n_f = fa; n_g = ga} = Nodetab.find nodetab psa
      and {n_f = fb; n_g = gb} = Nodetab.find nodetab psb
      in fa < fb || (fa=fb && ga>=gb) in

    let openlist = OpenList.make order Options.astar_openlist_initial_size in

    (* reconstruct the solution by back-traversing
       the nodes from the goal to the start *)
    let solution ps =
      let rec loop ps solution =
        let {n_g = g; n_parent = maybe_psp} = Nodetab.find nodetab ps in
        let solution = (P.unpack ps, g)::solution
        in match maybe_psp with
          Some pp -> loop pp solution
        | None -> solution
      in loop ps [] in

    (* insert children of the expanded node into the open list *)
    let rec insert children = match children with
         [] ->
           if OpenList.is_empty openlist then [] (* no solution *)
           else let ps = OpenList.extract_first openlist in
                let s = P.unpack ps
                in ( onExtract s
                   ; expand s ps [] )
       | (sc, gc, mknc)::children ->
         let psc = P.pack sc
         in if Nodetab.mem nodetab psc then
              (* reinsert only if reached through a shorter path *)
              let {n_g = gc'; n_f = fc'} as nc' = Nodetab.find nodetab psc
              in if gc < gc' then
                   ( Nodetab.replace nodetab psc
                       {nc' with n_g = gc; n_f = P.add gc (P.sub fc' gc')}
                   ; if OpenList.mem openlist psc then
                       ( OpenList.reorder_up openlist psc
                       ; insert children )
                     else maybe_expand_immediately sc psc children )
                 else insert children
            else ( inc_counter nodes_generated
                 ; Nodetab.add nodetab psc (mknc ())
                 ; maybe_expand_immediately sc psc children )

    (* optimization: if the child will be inserted
       at the head of openlist, expand immediately *)
    and maybe_expand_immediately sc psc children =
      if !Options.astar_expand_immediately &&
         (P.is_pgoal psc
          || (not (OpenList.is_empty openlist)
              && order psc (OpenList.first openlist))) then
        ( inc_counter nodes_imexed
        ; expand sc psc children )
      else ( OpenList.add openlist psc
           ; onInsert sc
           ; insert children )

    (* expand next node in the open list *)
    and expand s ps elder_children =
      if P.is_goal s then solution ps
      else
        let {n_f = f; n_g = g} = Nodetab.find nodetab ps in
        let children =
              List.map (fun (sc, dg) ->
                          let gc = P.add g dg in
                          let mknc () =
                            let hc = ( inc_counter heur_computed
                                     ; call_timed (fun () -> heur sc)
                                                  heur_total_time ) in
                            let fc = max f (P.add gc hc)
                            in {n_f = fc; n_g = gc; n_parent = Some ps}
                          in sc, gc, mknc)
                ( inc_counter nodes_expanded
                ; call_timed (fun () -> P.children s) children_total_time )
        in insert (List.rev_append children elder_children)

    in call_timed
         (fun () ->
            let pstate = P.pack state
            in ( reset_counters ()
               ; Nodetab.add nodetab pstate
                  {n_g = P.zero; n_f = P.zero; n_parent = None}
               ; expand state pstate [] ))
         search_time
end
