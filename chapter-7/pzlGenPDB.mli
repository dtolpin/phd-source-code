(* Genpdb has now public values, it's a command line tool
   for generating additive pattern databases for 15 puzzle.
   Usage:
   
       genpdb 1 4 5 8 9 12 13 > pattern.txt

   For generating the pattern database for tiles 1 4 5 8 9 12 13.
   The database is written to the standard output as lines of
   pairs "PERMUTATION  DISTANCE" where PERMUTATION is Perm.encode
   of the state, DISTANCE is the heuristic distance to the goal
   [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;0]. *)
