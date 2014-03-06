type t = int list

(* encode permutation as an integer code; must be efficient *)
let encode perm range =
  let rec compress tail mul code =
    match tail with
      [] -> code
    | num'::tail ->
      let num = 
        let rec loop num perm =
            match perm with
                [] -> failwith "cannot happen: digit not in permutation"
              | n::perm when n=num' -> num
              | n::perm when n<num' -> loop (num-1) perm
              | _::perm -> loop num perm
        in loop num' perm
      in compress tail (mul-1) (code*mul + num)
  in compress perm range 0

(* encode permutation as a an integer code; clear but slow *)
let encode_slow perm range =
  let rec compress tail mul code =
    match tail with
      [] -> code
    | num::tail ->
      let tail = List.map (fun n -> if n>num then pred n else n) tail
      in compress tail (mul-1) (code*mul + num)
  in compress perm range 0

(* restore permutation from an integer code; mostly for testing *)
let decode code range len =
  let rec expand perm code div = 
    if div>range then perm
    else 
      let num = code mod div in
      let perm = List.map (fun n -> if n>=num then succ n else n) perm
      in expand (num::perm) (code/div) (succ div)
  in expand [] code (range-len +1)

;;

(* tests *)

let check perm range = 
  let len = List.length perm in
  let code = encode perm range 
  in if perm <> decode code range len then 
      failwith ("perm: ["
                ^ (List.fold_left (fun a b -> a ^ "; " ^ (string_of_int b))
                     (string_of_int (List.hd perm))
                     (List.tl perm))
                ^ "] " ^ (string_of_int range))
in ( check [0] 1
   ; check [3] 6
   ; check [0;1] 2
   ; check [0;1;2;3] 4 
   ; check [5;4;3;2;1;0] 6
   ; check [3;0] 5 )
  
 
 
