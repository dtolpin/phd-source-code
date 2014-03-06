(* Operations on permutations *)

type t = int list (* a permutation is a list of integers *)

(* encode permutation as a an integer *)
val encode: t (* permuted numbers *) -> int (* range *) -> int (* code *)

(* decode permutation from integer *)
val decode: int (* code *) -> int (* range *) -> int (* len *) -> t

