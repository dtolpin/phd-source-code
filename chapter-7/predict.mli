(* predictors for the fixed point of monotoniously growing functions *)

(* a predictor accepts x and y and returns x such that x=y(x) *)
type predictor = (*x*) float -> (*y*) float -> float

(* makes linear predictor, accepts the starting point *)
val make_linear: (*x*) float -> (*y*) float -> predictor
