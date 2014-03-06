open Pzldef
open PzlHeurManhattan

let conflict state = 
  manhattan state
  + let rec loop i j d =
      if i=boardlen then d
      else if j=boardlen then loop (i+1) (i+2) d
      else if ((let lineno = state.(i)/sidelen
                in state.(j)/sidelen=lineno
                   && goal.(i)/sidelen=lineno
                   && goal.(j)/sidelen=lineno)
                || (let colno = state.(i) mod sidelen
                    in state.(j) mod sidelen=colno
                    && goal.(i) mod sidelen=colno 
                    && goal.(j) mod sidelen=colno))
              && (state.(i)>state.(j)) <> (goal.(i)>goal.(j))
           then loop (i+1) (i+2) (d+2)
           else loop i (j+1) d
    in loop 1 2 0
