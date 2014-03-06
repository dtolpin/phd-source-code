(* compare heuristics offline *)

open Pzldef
open PzlHeur

let sampling_gap = 65536

let collect heura heurb =
  let table = Hashtbl.create 17 
  and nperms = fpcount boardlen
  and goal_parity = Perm.encode (Array.to_list goal) boardlen mod 2
  in let rec loop iperm =
       if iperm >= nperms then table
       else begin
         if iperm mod 2 = goal_parity then begin
           let _ = if iperm mod 1000 = 0 
             then Printf.eprintf "%14d of %14d (%2.3f%%)\r" iperm nperms ((float iperm)*. 100.0/.(float nperms))
             else () in
           let state = Array.of_list (Perm.decode iperm boardlen boardlen) in 
           let ha = heura state
           and hb = heurb state
           in try
                let hb_min, hb_max = Hashtbl.find table ha
                in if hb < hb_min then Hashtbl.replace table ha (hb, hb_max)
                  else if hb > hb_max then Hashtbl.replace table ha (hb_min, hb)
             with Not_found -> 
               Hashtbl.replace table ha (hb, hb)
         end
         ; loop (iperm+(Random.int sampling_gap))
       end
     in loop 0

;;

if not !Sys.interactive then
  let () = Random.self_init () in
  let firstname = ref "manhattan" in
  let secondname = ref "pdb" in  
  let () = match Array.length Sys.argv with
      3 -> ( firstname := Sys.argv.(1)
           ; secondname := Sys.argv.(2) )
    | 2 -> ( secondname := Sys.argv.(1) )
    | 1 -> ( )
    | _ -> failwith "wrong number of args" in
  let firstheur = make_heur !firstname 
  and secondheur = make_heur !secondname in 
  let b_of_a = collect firstheur secondheur
  in ( Hashtbl.iter 
         (fun ha (hb_min, hb_max) -> Printf.printf "%5d: %5d %5d\n" ha hb_min hb_max)
         b_of_a )
