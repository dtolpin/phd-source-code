(* path-finding search abstraction *)

(* Search problem *)
module type Problem = sig
  (* defined by state, distance measure, generative model, goal test *)
  type state 
  type distance
  val children: state -> (state*distance) list
  val is_goal: state -> bool
  val degree: state -> int

  (* distances are additive *)
  val zero: distance  (* zero distance *)
  val one: distance   (* lower bound on the minimum distance *)
  val infty: distance (* upper bound on the maximum distance *)
  val add: distance -> distance -> distance
  val sub: distance -> distance -> distance
  val div: distance -> distance -> float

  (* in addition, for the sake of performance
     packed state and packing/unpacking functions
     are provided, so that states waiting in a list
     take less memory *)
  type pstate
  val pack: state -> pstate
  val unpack: pstate -> state

  (* optimization - it is often easier to check on packed
     state whether it is a goal *)
  val is_pgoal: pstate -> bool
    
  (* packed states are stored in fast lookup structures *)
  type t = pstate
  val equal: t -> t -> bool
  val hash: t -> int
end

(* Type aliases for concepts *)
(*   a path is a sequence of states *)
type ('s, 'd) path = ('s*'d) list

(*   a heustic takes a state and returns
     an estimate of the distance to the goal *)
type ('s, 'd) heuristic = 's -> 'd
