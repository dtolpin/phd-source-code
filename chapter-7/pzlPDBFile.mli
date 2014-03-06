(* pdb file abstraction *)

(* text file *)
val write_txt_header: out_channel -> unit
val write_txt_entry: out_channel -> int -> int -> unit
val read_txt_entry: in_channel -> (int -> int -> 'a) -> 'a

(* compiled binary file *)
val write_bin_entry: out_channel -> int -> int -> unit
val read_bin_entry: in_channel -> (int -> int -> 'a) -> 'a
