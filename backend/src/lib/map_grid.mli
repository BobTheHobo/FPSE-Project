open Core

module Params : sig
  type t = { b : int; s1 : int; s2 : int } [@@deriving sexp, compare]
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

module type MAP_GRID = sig
  type 'a t = 'a Coordinate.CoordinateMap.t [@@deriving sexp]

  val empty : 'a t
  val to_string : 'a t -> string
  val handle_collisions : 'a t -> 'a t
  val coordinate_set : 'a t -> Coordinate.CoordinateSet.t
  val next : 'a t -> width:int -> height:int -> 'a t
end

module Make (CellType: CELL_TYPE) : sig
  type t = CellType.TSet.t Coordinate.CoordinateMap.t [@@deriving sexp]

  val empty : t
  val to_string : t -> string
  val coordinate_set : t -> Coordinate.CoordinateSet.t
  val handle_collisions : t -> t
  val next : t -> width:int -> height:int -> t
end
