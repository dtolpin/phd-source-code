(* predictors for the fixed point of monotonously growing functions *)
type predictor = float -> float -> float

(* linear predictor with plato correction:
   if there is a plato, predict relative to
   a point before the plato:
     (1,5), (3,6) -> 9, (4,6) -> 7 (rather than 6) *)
let make_linear x_0 y_0 =
  let x_a = ref x_0 and y_a = ref y_0            (* base point *)
  and x_b = ref x_0 and y_b = ref y_0              (* midpoint *)
  and x_min = ref x_0                                (* last x *)
  and pred = ref (max x_0 y_0) in           (* last prediction *)
  let predictor x y =
    if x < !x_min then !pred         (* update pred only when x advances *)
    else 
      ( x_min := x
      ; if y > !y_b then               (* update base point when y grows *)
          ( x_a := !x_b
          ; y_a := !y_b
          ; x_b := x
          ; y_b := y )
      ; let k = (y -. !y_a) /. (x -. !x_a)
        in ( if (k <> 1.0) && ((y > x) = (k < 1.0)) then
               pred := (y -. k *. x) /. (1.0 -. k)
             else  
               pred := max !pred (max x y)  (* when k doesn't make sense *)
           ; !pred ) )         
  in predictor
