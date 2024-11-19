open Core

type t = { x : int; y : int } [@@deriving sexp, compare, equal]

let to_string (t : t) = Sexp.to_string (sexp_of_t t)

let b = 3
let s1 = 2
let s2 = 3