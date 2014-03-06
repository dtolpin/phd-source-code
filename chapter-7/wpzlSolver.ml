(* solver for 15-puzzles *)

module type PzProblem = sig
  type state = Pzldef.state
  type distance

  val children: state -> (state*distance) list
  val is_goal: state -> bool
  val degree: state -> int

  val zero: distance
  val one: distance
  val infty: distance
  val add: distance -> distance -> distance
  val sub: distance -> distance -> distance
  val div: distance -> distance -> float
  val string_of_distance: distance -> string

  type pstate = Pzldef.pstate
  val pack: state -> pstate
  val unpack: pstate -> state

  val is_pgoal: pstate -> bool
  type t = pstate
  val equal: t -> t -> bool
  val hash: t -> int
end

module Make(PzProblem: PzProblem) =  struct
(* simple solver for puzzles *)

  open Stat

  module PzAstar = Astar.Make(PzProblem)
  module PzIDAstar = Idastar.Make(PzProblem)
  module PzLazyA = LazyA.Make(PzProblem)
  module PzLida = Lida.Make(PzProblem)

  let quiet = ref false            (* when true, no ongoing output on stderr *)
  let summary = ref false          (* when true, no shortest path on stdout *)
  let heur_time_multiplier = ref 1  (* heuristic computation takes that many times more time *)

  let algname = ref "astar"
  let heurname = ref "manhattan"
  let heurname' = ref "conflict"
  
(* make algorithm from algorithm name; an algorithm is a function
   that accepts the initial state and returns the shortest path *)
  let algorithm algname make_heur:
      Pzldef.state -> (PzProblem.state, PzProblem.distance) Search.path  = 

  (* wrap the heuristic function to print progress report *)
    let max_h = ref PzProblem.infty in
    let wrap_heur name =
      let heur = make_heur name
      in fun state ->
        let h = ( for i = 1 to !heur_time_multiplier - 1 do
                    ignore (heur state)
                  done
                ; heur state )
        in ( if not !quiet && h < !max_h then
               ( max_h := h
               ; Printf.eprintf "\r%%\th=%s: %s/exp=%d "
                 (PzProblem.string_of_distance h)
                 (Pzldef.string_of_state state) !nodes_expanded
               ; if !number_of_iterations != 0 then
                   Printf.eprintf "nit=%d " !number_of_iterations
               ; flush stderr 
               ; if !max_h = PzProblem.zero then max_h := PzProblem.infty )
           ; h )

    in match Str.split (Str.regexp ":") algname with
      ["astar"] ->
        PzAstar.shortest_path (wrap_heur !heurname)
          
    | ["lazya"] ->
      PzLazyA.shortest_path (wrap_heur !heurname) (wrap_heur !heurname')

    | ["lazya/voi"] ->
      PzLazyA.shortest_path
        ~worth:PzLazyA.worth
        (wrap_heur !heurname) (wrap_heur !heurname')

    | ["idastar"] ->
      PzIDAstar.shortest_path (wrap_heur !heurname)

    | ["lida"] ->
      PzLida.shortest_path (wrap_heur !heurname) (wrap_heur !heurname')

    (*
     * lida/voi...: reinitialize the predictor on every instance
     *)

    | "lida/voi"::args ->
      let heur = (wrap_heur !heurname) in
      let heur' = (wrap_heur !heurname')
      in fun state ->
          let learn, worth = PzLida.predictor 
            ( match args with
              | ["f"; prob] -> Lida.Fixed (float_of_string prob)
              | ["b"; pmax] -> Lida.Bounded (float_of_string pmax)
              | [] | ["b"] -> Lida.Bounded 1.
              | _ -> assert false )
          in PzLida.shortest_path ~learn:learn ~worth:worth heur heur' state

    | _ -> invalid_arg algname

(* print the solution *)
  let print_solution path =
    match path with
      [] -> Printf.printf "FAILURE\n"
    | _ -> 
      let d = List.length path - 1 in
      let _, f = List.nth path d in
      let print_step (s, f) =
        Printf.printf "# %s: %s\n"
          (Pzldef.string_of_state s) (PzProblem.string_of_distance f)
      in ( if not !quiet then (* visually separate solution from progress reporting *)
             ( Printf.eprintf "\n"; flush stderr ) 
         ; Printf.printf "d=%d f=%s %s\n"
             d (PzProblem.string_of_distance f) (string_of_stat ())
         ; if not !summary then 
             List.iter print_step path )
      
  ;;

  exception Exit of int
  
  let main make_heur argv =
  (* helper: get instance as array of strings representing tiles on board,
     convert to state, solve, print summary and path *)
    let read_eval_print solve args =
      let state = Pzldef.state_of_board (Array.map int_of_string args) in
      let path = if Pzldef.is_solvable state then solve state else []
      in print_solution path in
      
    let usage = Printf.sprintf
      "Solves 15-puzzle:
\t%s [option ...] [board]
accepts either a single board position on the command line,
or a numbered list of board positions on standard input.
A board is a space-separated list of integers.
The options are:"
      argv.(0) in

    let anonymous _ =
      if Array.length argv - !Arg.current <> Pzldef.boardlen then
        ( Printf.eprintf "%s\n" usage
        ; raise (Exit 1) )
      ; read_eval_print (algorithm !algname make_heur)
        (Array.sub argv !Arg.current Pzldef.boardlen)
      ; raise (Exit 0) in
    
    let options = 
      [ "-a", Arg.Set_string algname,   "<name>       algorithm name - one of 'astar', 'lazya', 'lazya/voi',\n                'idastar', 'lida' 'lida/voi', 'lida/voi:b', 'lida/voi:f:<prob>'";
        "-h", Arg.Set_string heurname,  "<name>       primary (or the only) heuristic name - one of 'manhattan',\n                'conflict', 'pdb', 'pdb555', 'pdb78', or derived heuristics\n                '+' using (for maximum of two heuristics) and '^' for lookahead\n                to the specified depth; pdb heuristics are available for\n                unweighted 15-puzzle only";
        "-H", Arg.Set_string heurname', "<name>       secondary heuristic name, see -h for the list of accepted\n                values";
        "-m", Arg.Set_int heur_time_multiplier, "<times>      heuristic computation takes that many times longer";
        "-q", Arg.Set quiet,            "             suppress progress reporting on stderr";
        "-s", Arg.Set summary,          "             skip the path, print summary only";
        "--H-computed-prior", Arg.Set_int Options.lazya_heur'_computed_prior,     "    prior number of computations of the secondary\n                         heuristic (Lazy A*)";
        "--H-helpful-prior", Arg.Set_int Options.lazya_heur'_helpful_prior,      "     prior number of helfpul computations of the secondary\n                         heuristic (Lazy A*)";
        "--heuristic-bypassing", Arg.Set Options.lazya_heuristic_bypassing,  " prune the second heuristic when irrelevant (Lazy A*)";
        "--no-immediate-expand", Arg.Clear Options.astar_expand_immediately, " do not expand immediately (A*, Lazy A*)";
        "--min-loop-size", Arg.Set_int Options.idastar_min_loop_size, (Printf.sprintf "       minimum loop size, defaults to %d (IDA*)" !Options.idastar_min_loop_size)]

    in Arg.parse_argv argv options anonymous usage
    (* read states from standard input *)
    ; let solve = algorithm !algname make_heur in 
      let rec lines () =                   
        try 
          let line = input_line stdin in
          let words = Str.split (Str.regexp "[ \t\n\r]+") line
          in ( match words with
            [] -> ()
          | word::_ when word.[0] = '#' -> ()
          | id::words -> 
            ( Printf.printf "%s: " id
            ; flush stdout
            ; if not !quiet then 
                ( Printf.eprintf "\r%% %-6s" id
                ; flush stderr )
            ; read_eval_print solve (Array.of_list words) ) )
          ; lines ()
        with End_of_file -> () 
      in lines ()
end

module PzlSolver = Make(PzlProblem)
module WpzSolver = Make(WpzProblem)
module MpzSolver = Make(MpzProblem)

let is_solvepzl s = Str.string_match (Str.regexp ".*\\bsolvepzlx?") s 0
let is_solvewpz s = Str.string_match (Str.regexp ".*\\bsolvewpzx?") s 0
let is_solvempz s = Str.string_match (Str.regexp ".*\\bsolvempzx?") s 0

;;

if not !Sys.interactive then
  let main =
    if is_solvepzl Sys.argv.(0) then PzlSolver.main PzlHeur.make_heur
    else if is_solvewpz Sys.argv.(0) then WpzSolver.main WpzHeur.make_heur
    else if is_solvempz Sys.argv.(0) then MpzSolver.main MpzHeur.make_heur
    else failwith ( "expected executable file name is "
                    ^ "either solvepzl[x] or solvewpz[x]" )
  in try
       ( Arg.current := 0
       ; main Sys.argv )
     with
         PzlSolver.Exit code -> exit(code)
       | (Arg.Bad message| Arg.Help message) -> prerr_string message
  
