(* statistics *)
let search_time = ref 0.0
let nodes_expanded = ref 0
let upg_nodes_expanded = ref 0
let nodes_generated = ref 0
let nodes_imexed = ref 0
let children_total_time = ref 0.0
let heur_computed = ref 0 
let heur_bypassed = ref 0
let heur_total_time = ref 0.0
let heur'_computed = ref 0
let heur'_bypassed = ref 0
let heur'_total_time = ref 0.0
let number_of_iterations = ref 0

let reset_counters () =
  search_time := 0.0;
  nodes_expanded := 0;
  upg_nodes_expanded := 0;
  nodes_generated := 0;
  nodes_imexed := 0;
  children_total_time := 0.0;
  heur_computed := 0;
  heur_bypassed := 0;
  heur_total_time := 0.0;
  heur'_computed := 0;
  heur'_bypassed := 0;
  heur'_total_time := 0.0;
  number_of_iterations := 0

let inc_counter counter =
  counter := !counter + 1

let call_timed thunk total_time = 
      let time_before = (Unix.times ()).Unix.tms_utime in
      let ret = thunk () in
      let time_after = (Unix.times ()).Unix.tms_utime
      in ( total_time := !total_time +. time_after -. time_before
         ; ret )

let string_of_stat () =
  let s = Printf.sprintf 
    "exp=%d,%d gen=%d,%d h=%d+%d H=%d+%d utime=%6f(tc=%6f,th=%6f,tH=%6f)"
    !nodes_expanded !upg_nodes_expanded !nodes_generated !nodes_imexed
    !heur_computed !heur_bypassed !heur'_computed !heur'_bypassed
    !search_time !children_total_time !heur_total_time !heur'_total_time 
  in if !number_of_iterations != 0 then
      (Printf.sprintf "nit=%d " !number_of_iterations) ^ s
    else s

