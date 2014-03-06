(* definitions for N-puzzle *)

(* board size *)
val sidelen: int
val boardlen: int

(* board state: inverted state *)
type pstate = int64         (* 4 bits per tile *)
type state = int array      (* int per tile *)
type board = int array      (* inverse of state *)

type pattern = int list        (* pattern *)

(* pack and unpack the state *)
val pack: state -> pstate
val unpack: pstate -> state

(* convert between state and board *)
val board_of_state: state -> board
val state_of_board: board -> state

(* string representation of state *)
val string_of_state: state -> string

(* goal, the blank is in the last position *)
val goal: state

(* whether the state is ever solvable *)
val is_solvable: state -> bool

(* compute out-degree of the state *)
val degree: state -> int

(* generate the list of children of the pstate,
   the second element is the moved tile *)
val children: state -> (state*int) list

(* compute unique integer representing the state, good for hashing *)
val footprint: state -> pattern -> int

(* compute total count of footprints given pattern length *)
val fpcount: int -> int

(* heuristic function, gets state and returns distance to the goal *)
type 'a heuristic = state -> 'a
