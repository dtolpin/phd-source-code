(* Lazy A* search algorithm *)

open Stat

module type A = sig
  type state
  type distance
  val shortest_path:
    ?worth: (state -> bool) ->
    (state, distance) Search.heuristic ->
    (state, distance) Search.heuristic ->
    state -> (state, distance) Search.path

  val worth: state -> bool
end

module Make(P: Search.Problem) = struct
  type state = P.state
  type distance = P.distance

  (* collections for open and closed `lists' *)
  type node = {n_f: distance; n_g: distance;

               (* for heuristic bypassing *)
               n_h: distance*distance;  (* lower and upper bound for h *)
               n_h': distance*distance; (* lower and upper bound for h' *)

               n_upg: bool; (* true when the node was 'upgraded'
                               by computing the auxiliary heuristic *)
               n_parent: P.pstate option}

  module Nodetab = Hashtbl.Make(P) (* of nodes *)
  module OpenList = Base.PriorityQueue.Make(P) (* shortest first *)

  let shortest_path ?(worth = fun _ -> true) heur heur' state =
    (* wrap functions to collect statistics *)
    let nodetab = Nodetab.create Options.astar_closedlist_initial_size in

    (* ord a b = true means that the priority of b is not higher
       than the priority of a *)
    let order psa psb =
      let {n_f = fa; n_g = ga; n_upg = upga} = Nodetab.find nodetab psa
      and {n_f = fb; n_g = gb} = Nodetab.find nodetab psb
      in fa < fb || (fa=fb && (ga>gb || (ga=gb && upga))) in

    let openlist = OpenList.make order Options.astar_openlist_initial_size in

    (* reconstruct the solution by back-traversing
       the nodes from the goal to the start *)
    let solution ps =
      let rec loop ps solution =
        let {n_g = g; n_parent = maybe_psp} = Nodetab.find nodetab ps in
        let solution = (P.unpack ps, g)::solution
        in match maybe_psp with
          Some psp -> loop psp solution
        | None -> solution
      in loop ps []  in

    let rec insert children =
       match children with
         [] ->
           if OpenList.is_empty openlist then [] (* no solution *)
           else let ps = OpenList.first openlist in
                let s = P.unpack ps
                in expand ~immediately:false s ps []
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
         (P.is_pgoal psc ||
            (not (OpenList.is_empty openlist)
             && order psc (OpenList.first openlist))) then
        ( inc_counter nodes_imexed
        ; expand ~immediately:true sc psc children )
      else ( OpenList.add openlist psc
           ; insert children )

    (* expand next node in the open list, or a newborn child immediately *)
    and expand ~immediately s ps elder_children =
      if P.is_goal s then solution ps
      else
        let {n_g = g; n_f = f;
             n_h = hl, hu; n_h' = h'l, h'u;
             n_upg = upg}
            as n = Nodetab.find nodetab ps
        in if upg || not (worth s) then
             let () = if not immediately then OpenList.remove_first openlist in
             let children =
               List.map (fun (sc, dg) ->
                           let gc = P.add g dg in
                           let mknc () =
                             (* heuristic bypassing *)
                             let hc, hcl, hcu = 
                               if !Options.lazya_heuristic_bypassing 
                                 && P.add hu dg <= P.sub h'l dg 
                               then ( inc_counter heur_bypassed
                                    ; P.sub h'l dg, P.sub hl dg, P.add hu dg )
                               else let hc = call_timed (fun () -> heur sc)
                                                        heur_total_time in 
                                    ( inc_counter heur_computed
                                    ; hc, hc, hc ) in
                             let upgc = 
                               if !Options.lazya_heuristic_bypassing
                                 && hc >= P.add h'u dg
                               then ( inc_counter heur'_bypassed 
                                    ; true )
                               else false
                             in {n_f = max f (P.add gc hc); n_g = gc; 
                                 n_h = hcl, hcu;
                                 n_h' = P.sub h'l dg, P.add h'u dg;
                                 n_upg = upgc;
                                 n_parent = Some ps}
                           in sc, gc, mknc)
                 ( inc_counter nodes_expanded
                 ; if upg then inc_counter upg_nodes_expanded
                 ; call_timed (fun () -> P.children s) children_total_time )
             in insert (List.rev_append children elder_children)

           else
             let h' = call_timed (fun () -> heur' s) heur'_total_time in
             let f' = P.add g h'
             in ( inc_counter heur'_computed
                ; Nodetab.replace nodetab ps
                    {n with n_f = max f f'; n_h' = (h', h'); n_upg = true}
                ; if f' > f then
                    ( if immediately then
                        OpenList.add openlist ps
                      else OpenList.reorder_down openlist ps
                    ; insert elder_children )
                  else expand immediately s ps elder_children )

    in call_timed
         (fun () ->
            let pstate = P.pack state
            in ( reset_counters ()
               ; Nodetab.add nodetab pstate
                   {n_g = P.zero; n_f = P.zero;
                    n_h = (P.zero, P.infty); n_h' = (P.zero, P.infty);
                    n_upg = false; n_parent = None}
               ; expand ~immediately:true state pstate [] ))
         search_time

  let worth state =
    let n = !heur_computed + !heur_bypassed  in
    let n' = !heur'_computed + !heur'_bypassed in
    let n'h = n' - !upg_nodes_expanded in
    let tr = if !heur'_total_time <= min_float
        || !heur_total_time <= min_float then 1.0 
      else (float_of_int !Options.lazya_heur'_computed_prior +. 
              !heur'_total_time /. !heur_total_time *. float_of_int n)
        /. float_of_int (!Options.lazya_heur'_computed_prior + n') in
    let pH = float_of_int (n'h + !Options.lazya_heur'_helpful_prior)
      /. float_of_int (n' + !Options.lazya_heur'_computed_prior)
    in float_of_int (P.degree state) > tr /. (pH *. ( 1.0 +. tr))

end
