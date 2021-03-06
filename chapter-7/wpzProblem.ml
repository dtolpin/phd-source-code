type state = Pzldef.state
type distance = int
let children = Pzldef.children (* the second element in tuple is already the tile *)
let is_goal = (=) Pzldef.goal 
let degree = Pzldef.degree
  
let zero = 0
let one = 1
let infty = 2000
let add = (+)
let sub = (-)
let div x y = float_of_int x/.float_of_int y
let string_of_distance = string_of_int

type pstate = Pzldef.pstate
let pack = Pzldef.pack 
let unpack = Pzldef.unpack

let is_pgoal =
  let pgoal = pack Pzldef.goal
  in fun pstate -> pstate = pgoal

type t = pstate
let equal = (=)
let hash = Hashtbl.hash
