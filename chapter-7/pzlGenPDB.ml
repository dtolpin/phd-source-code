(* N-puzzle Pattern Database Generator; needs OCaml Batteries *)

module File = BatFile
module IO = BatIO
module BitSet = BatBitSet

open Pzldef
open PzlPDBFile

(* output heuristics *)
let print_heur_header () =
  write_txt_header stdout 
let print_heur node pattern level =
  write_txt_entry stdout (footprint node pattern) level

module Tape: sig
  type t
  exception Empty
  exception Inuse (* attempt to write to non-empty tape *)
  val make: unit -> t
  val empty: t -> bool
  val add: pstate -> t -> unit
  val take: t -> pstate
  val length: t -> int
  val eject: t -> unit
end = struct
  (* disk based tapes to conserve memory *)
  type channel = Write of unit IO.output | Read of IO.input
  type t = { filename: string; 
             mutable count: int; 
             mutable channel: channel; }

  exception Empty
  exception Inuse

  let empty tape = tape.count=0

  let make () = 
    let filename = Filename.temp_file "pdb" ".tape";
    in { filename = filename;
         count = 0;
         channel = Write (File.open_out filename) }

  let rec add pstate tape =
    match tape.channel with
      Write stream -> 
        ( IO.write_i64 stream pstate
        ; tape.count <- tape.count+1 )
    | Read stream ->
        ( if tape.count <> 0 then raise Inuse
        ; IO.close_in stream
        ; tape.channel <- Write (File.open_out tape.filename)
        ; add pstate tape )

  let rec take tape =
    match tape.channel with
      Read stream ->
        ( if tape.count=0 then raise Empty
        ; tape.count <- tape.count-1
        ; IO.read_i64 stream )
    | Write stream ->
        ( IO.close_out stream
        ; tape.channel <- Read (File.open_in tape.filename)
        ; take tape )

  let length tape = tape.count 

  let eject tape = 
    let () = Unix.unlink tape.filename in 
    match tape.channel with
      Read stream -> IO.close_in stream
    | Write stream -> IO.close_out stream
end

(* Single round of BFS *)
let bfs level pattern itape otape written visited =
  let phantoms = ref [] in (* masked-out children *)

  (* consume phantoms and input tape *)
  let rec loop_tape () =
    match !phantoms with
      node::nodes -> 
        ( phantoms := nodes; expand_node node )
    | [] ->
      if Tape.empty itape then ()
      else expand_node (unpack (Tape.take itape))

  and expand_node node =
    (* route childrens to either the output tape or the phantoms *)
    let fp_n_w = footprint node pattern in

    let rec loop_children children = 
        match children with
          child::children ->
            let fp_w = footprint child pattern
            and fp_v = footprint child (0::pattern)
            in if fp_n_w <> fp_w then                     (* pattern tile moved *)
                ( if not (BitSet.is_set written fp_w) then
                    ( print_heur child pattern level
                    ; BitSet.set written fp_w ) 
                ; if not (BitSet.is_set visited fp_v) then 
                    ( Tape.add (pack child) otape
                    ; BitSet.set visited fp_v ) )

              else if not (BitSet.is_set visited fp_v) then (* dummy tile moved *)
                ( phantoms := child::!phantoms
                ; BitSet.set visited fp_v ) 

            ; loop_children children 

        | [] -> loop_tape ()
    in loop_children (List.map (fun (ch, _) -> ch) (children node))
  in loop_tape ()
  
(* generate the pattern database *)
let genpdb pattern =
  let itape = Tape.make ()
  and otape = Tape.make () in

  let written = BitSet.create (fpcount (List.length pattern))
  and visited = BitSet.create (1 + fpcount (List.length pattern)) in

  let rec loop_level level itape otape: unit = 
    if Tape.empty itape then ()
    else ( bfs level pattern itape otape written visited
         ; Printf.eprintf "STAT: LEVEL#%-2d %10d STATES\n" level (Tape.length otape)
         ; flush stderr
         ; loop_level (level+1) otape itape  )
  in ( print_heur_header ()
     ; Tape.add (pack goal) itape
     ; BitSet.set written (footprint goal pattern)
     ; loop_level 1 itape otape 
     ; Tape.eject itape 
     ; Tape.eject otape )
  
;;

if not !Sys.interactive then
  try 
    let pattern = List.map int_of_string (List.tl (Array.to_list Sys.argv))
    in genpdb pattern 
  with Failure _ -> 
    Printf.eprintf
      "Usage:\n\
\t%s MASK\nwhere MASK are space separated tile indices, for example\n\
\t%s 1 4 5 8 9\n"
      Sys.argv.(0) Sys.argv.(0)
