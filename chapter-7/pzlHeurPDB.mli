(* additive pattern database heuristic for 15-puzzle *)

(* gets a list of tuples (pattern,filename) and returns the heuristic *)
val makepdb_of_defs: (Pzldef.pattern*string) list -> int Pzldef.heuristic

(* gets a list of patterns and returns the heuristic;
   - assumes that the database for pattern [a;b;c;d]
     is in file pdbdir/a-b-c-d.pdb;
   - pdbdir defaults to the current directory *)
val makepdb: ?pdbdir:string -> Pzldef.pattern list -> int Pzldef.heuristic
