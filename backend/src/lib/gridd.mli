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

module Make : functor (Cell : Cell) -> sig
  module State : State with type elt = Cell.t
  
  type t = { cells : State.t; width : int; height : int }
  
  val empty : t
  
  val neighbors : State.elt -> width : int -> height : int -> State.t
  
  val next : t -> t
  val create : State.elt list -> width : int -> height : int -> t
end