module type Cell = sig
  type t [@@deriving sexp, compare, equal]
  val x : t -> int
  val y : t -> int
  val to_string : t -> string

  val b : int
  val s1 : int
  val s2 : int
end

module type State = sig
  type elt
  type t
  
  val to_string : t -> string
  val empty : t
  val add : t -> elt -> t
  val fold : t -> init : 'acc -> f:('acc -> elt -> 'acc) -> 'acc
  val survive_set : t -> t
  val spawn_set : t -> t
end

module Make (Cell : Cell) = sig
end