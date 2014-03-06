(* pdb file abstraction *)

(* text file *)
let write_txt_header outp =
  Printf.fprintf outp "%12s %12s\n" "STATE" "DISTANCE"
let write_txt_entry outp footprint distance =
  Printf.fprintf outp "%12d %12d\n" footprint distance
let read_txt_entry inp =
  Scanf.fscanf inp " %d %d"

(* compiled binary file *)
let write_bin_entry outp footprint distance =
  ( output_binary_int outp footprint
  ; output_byte outp distance )
let read_bin_entry inp f = 
    let footprint = input_binary_int inp in
    let distance = input_byte inp in
    f footprint distance
