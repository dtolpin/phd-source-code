(* available heuritics for 15 puzzle *)

(* Accepted names:
     manhattan
     conflict
     pdb555
     pdb78
   Operators:
     A+B - combined heuristic max(A, B),
           for example conflict+pdb555
     A^n - heuristic A with lookahead at depth n,
           for example manhattan^4 *)

val make_heur: string -> int Pzldef.heuristic
