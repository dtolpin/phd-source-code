open Pzldef

let manhattan =

  let distance = Array.make_matrix boardlen boardlen min_int in
  
  for tile = 1 to boardlen-1 do
    for pos = 0 to boardlen-1 do
      distance.(tile).(pos) <- tile *
        (abs(pos mod sidelen - goal.(tile) mod sidelen)
         + abs(pos / sidelen - goal.(tile) / sidelen))
    done
  done;

  fun state ->
    let rec loop i d = 
      if i = boardlen then d
      else loop (i+1) (d + distance.(i).(state.(i)))
    in loop 1 0 (* i=1 is the position of blank, skip *)

