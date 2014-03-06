(* Linear conflict heuristic for 15-puzzle.

   Linear Conflict Tiles Definition: Two tiles tj and tk are
   in a linear conflict if tj and tk are in the same line,
   the goal positions of tj and tk are both in that line,
   tj is to the right of tk and goal position of tj is to
   the left of the goal position of tk.

   The linear conflict adds at least two moves to the Manhattan
   Distance of the two conflicting tiles, by forcing them to
   surround one another. Therefore the heuristic function will
   add a cost of 2 moves for each pair of conflicting tiles.

   The Linar Conflict Heuristic is admissible. *)

val conflict: int Pzldef.heuristic
