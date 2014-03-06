open PzlPDBFile

(* see genpdb.ml for the text pattern database format *)
let rec loop footprint distance =
  ( write_bin_entry stdout footprint distance
  ; read_txt_entry stdin loop )

;;

if not !Sys.interactive then
  try
    ( ignore (input_line stdin) (* skip the header *)
    ; read_txt_entry stdin loop )
  with End_of_file -> ()
