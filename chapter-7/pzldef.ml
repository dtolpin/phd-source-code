(* definitions for N-puzzle *)

(* board size *)
let sidelen = 4
let boardlen = sidelen*sidelen

(* board state: inverted state *)
type pstate = int64         (* 4 bits per tile *)
type state = int array        (* positions of tiles; int per tile *)
type board = int array        (* tiles in positions; inverse of state *)

type pattern = int list        (* pattern *)

let pack state: pstate =
  let rec loop packed i =
      if i=boardlen then packed
      else loop (Int64.logor (Int64.shift_left packed 4) (Int64.of_int state.(i))) (i+1)
  in loop Int64.zero 0

let unpack pstate: state = 
  let state = Array.make boardlen 0
  in let rec loop pstate i =
       ( state.(i) <- Int64.to_int (Int64.logand pstate (Int64.of_int 0xF))
       ; if i=0 then state
         else loop (Int64.shift_right_logical pstate 4) (i-1) )
     in loop pstate (boardlen-1)

(* converting between state and board by invertion: *)
(* returns state to board *)
let board_of_state state =
  let board = Array.make boardlen 0
  in Array.iteri (fun i t -> board.(t) <- i) state
  ; board

(* returns board to state *)
let state_of_board = board_of_state

(* returns space-separated list of tile positions *)
let string_of_state s =
  String.concat " " (List.map string_of_int (Array.to_list s))

(* in the goal pstate the blank is in the first square *)
let first_is_blank = true

let goal = 
  if first_is_blank then (* first is blank *)
    Array.init boardlen (fun i -> i)
  else                    (* last is blank *)
    Array.init boardlen (fun i -> (i+boardlen-1) mod boardlen)

(* compute state polarity to determine solvability;
   see http://mathworld.wolfram.com/15Puzzle.html for details and references *)
let is_even state = 
  let board = Array.make boardlen 0
  in Array.iteri (fun i pos -> board.(pos) <- i) state
  ; let rec tile i n =
      let rec inversions j n =
        if j = boardlen then n
        else if board.(i) > board.(j) && board.(j) > 0 then inversions (j+1) (n+1)
        else inversions (j+1) n
      in if i = boardlen then n
        else if board.(i) <= 1 then tile (i+1) n
        else tile (i+1) (inversions (i+1) n) in 
    let ninv = tile 0 0
    in ((sidelen mod 2 = 1) && (ninv mod 2 = 0))
    || ((sidelen mod 2 = 0) && ((ninv mod 2 = 0) = (state.(0) / sidelen mod 2 = 1)))

(* a state is solvable if the state and the goal are of the same polarity *)
let is_solvable state = (is_even state) = (is_even goal)

(* compute out-degree of the state *)
let degree state =
  let bpos = state.(0) in

  let up d = if bpos >= sidelen           then d+1 else d
  and dn d = if bpos < boardlen-sidelen   then d+1 else d
  and lf d = if bpos mod sidelen <> 0     then d+1 else d
  and rt d = if (bpos+1) mod sidelen <> 0 then d+1 else d

  in (up (dn (lf (rt 0))))

(* generate the list of unpacked children of the pstate *)
let children state = 
  let bpos = state.(0) in

  (* generate the child by switching tpos with the blank tile *)
  let child tpos =
    let state = Array.copy state in
    let rec loop i = (* array lookup: find value of tile at tpos *)
      if state.(i)=tpos then
        (* switch the tile with the blank *)
        ( state.(i) <- bpos 
        ; state.(0) <- tpos
        ; state, i ) 
      else loop (i+1)
    in loop 0 in

  (* move in four directions, but don't hit walls *)
  let up cl = if bpos >= sidelen           then (child (bpos-sidelen))::cl else cl
  and dn cl = if bpos < boardlen-sidelen   then (child (bpos+sidelen))::cl else cl
  and lf cl = if bpos mod sidelen <> 0     then (child (bpos-1))::cl else cl
  and rt cl = if (bpos+1) mod sidelen <> 0 then (child (bpos+1))::cl else cl

  (* generate the list of all children *)
  in (up (dn (lf (rt []))))

(* masked footprint of the pstate, for set membership *)
let footprint state pattern: int = 
  Perm.encode (List.map (fun i -> state.(i)) pattern) boardlen

(* compute total count of footprints given pattern length *)
let fpcount patlen =
  Perm.encode 
    (let rec loop perm i =
       if i = boardlen then perm else loop (i::perm) (i+1)
     in loop [] (boardlen - patlen))
    boardlen
  + 1

type 'a heuristic = state -> 'a
