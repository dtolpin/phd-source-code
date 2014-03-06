(* available heuritics for 15 puzzle *)

open PzlHeurManhattan
open PzlHeurConflict
open PzlHeurPDB

(* heuristic creation is memoized to avoid reloading pattern databases *)

(* maximum of two heuristics *)
let max_heur ha hb = fun state -> max (ha state) (hb state)

module LA = Lookahead.Make(PzlProblem)

let rec make_heur' name =
  match name with
    "manhattan" -> manhattan
  | "conflict" -> conflict
  | ("pdb" | "pdb555") -> (makepdb [[1;2;3;4;5];[6;7;8;9;10];[11;12;13;14;15]])
  | "pdb78" -> (makepdb [[1;4;5;8;9;12;13];[2;3;6;7;10;11;14;15]])
  | name  when String.contains name '+' ->
    let k = String.index name '+' in
    let aname = String.sub name 0 k 
    and bname = String.sub name (k+1) (String.length name - k - 1) 
    in max_heur (make_heur aname) (make_heur bname)
  | name when String.contains name '^' ->
    let k = String.index name '^' in
    let basename = String.sub name 0 k
    and depth = int_of_string (String.sub name (k+1) (String.length name - k - 1))
    in LA.distance_to_goal (make_heur basename) depth
  | _ -> invalid_arg name

and make_heur = 
  let heurs = ref [] in
  fun name -> 
    try
      List.assoc name !heurs
    with Not_found -> 
      let heur = make_heur' name 
      in ( heurs := (name, heur)::!heurs
         ; heur )


