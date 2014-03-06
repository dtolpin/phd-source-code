open Pzldef
open PzlPDBFile

(* load a pdb from file into memory *)
let load (pattern, filename) =
  let pdb = Array.make (fpcount (List.length pattern)) 0  in 
  let file = open_in filename in
  let rec loop footprint distance =
    ( pdb.(footprint) <- distance
    ; read_bin_entry file loop )
  in try read_bin_entry file loop
    with End_of_file ->
      ( close_in file 
      ; pattern, pdb )

let makepdb_of_defs pdbdefs =
  let pdbs = List.map load pdbdefs 
  in fun state ->
    List.fold_left 
      (fun h (pattern,pdb) -> h + pdb.(footprint state pattern))
      0 pdbs

let makepdb ?(pdbdir="") ps =
  makepdb_of_defs
    (List.map 
       (fun p -> 
         (p,
          Filename.concat pdbdir 
            (String.concat "-" (List.map string_of_int p)) ^ ".pdb"))
       ps)


