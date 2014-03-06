type state = Pzldef.state
type distance = float
let children state = (* the weight is the inverse of the tile face value *)
  List.map (fun (s, i) -> s, 1. /. float(i)) (Pzldef.children state)
let is_goal = (=) Pzldef.goal 
let degree = Pzldef.degree

let zero = 0.
let one = 1. /. 15.
let infty = 80.0
let add = (+.)
let sub = (-.)
let div = (/.)
let string_of_distance = string_of_float

type pstate = Pzldef.pstate
let pack = Pzldef.pack 
let unpack = Pzldef.unpack

let is_pgoal =
  let pgoal = pack Pzldef.goal
  in fun pstate -> pstate = pgoal

type t = pstate
let equal = (=)
let hash = Hashtbl.hash
