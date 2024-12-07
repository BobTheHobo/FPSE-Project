open Core

module Params : sig
  type t = { b : int; s1 : int; s2 : int } [@@deriving sexp, compare]
end

module Coordinate : sig
  module T : sig
    type t = { x : int; y : int } [@@deriving sexp, compare]
  end

  include T
  module CSet : module type of Set.Make (T)

  type t = T.t
  type grid = { coordinates : CSet.t; width : int; height : int }

  val to_string : t -> string
  val spawn_set : grid -> b:int -> CSet.t
  val survive_set : grid -> s1:int -> s2:int -> CSet.t
  val is_neighbor : t -> t -> bool
end

module type CELL_TYPE = sig
  module T : sig
    type t [@@deriving sexp, compare]
  end

  include T
  module TSet : Set.S with type Elt.t := t

  val type_list : t list
  val to_string : t -> string
  val compare : t -> t -> int
  val params_of_t : t -> Params.t
  val handle_collisions : TSet.t -> t option
end

module Make (Cell_type : CELL_TYPE) : sig
  module CMap : Map.S with type Key.t = Coordinate.t

  type t = Cell_type.TSet.t CMap.t

  val empty : t
  val handle_collisions : t -> t
  val next : t -> width:int -> height:int -> t
end
