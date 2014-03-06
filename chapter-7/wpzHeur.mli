(* available heuritics for weighted 15 puzzle *)

(* Accepted names:
     manhattan
     conflict
   Operators:
     A+B - combined heuristic max(A, B),
           for example conflict+manhattan^4
     A^n - heuristic A with lookahead at depth n,
           for example manhattan^4 *)
val make_heur: string -> int Pzldef.heuristic
