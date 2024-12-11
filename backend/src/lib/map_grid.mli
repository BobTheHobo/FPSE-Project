open Core

module Params : sig
  type t = { b : int; s1 : int; s2 : int } [@@deriving sexp, compare]
end

module type CELL_TYPE = sig
  type t [@@deriving compare, sexp]
  module CellSet : Set.S with type Elt.t := t
  
  val all_set : CellSet.t 
  val cell_ls : t list
  val to_string : t -> string
  val compare : t -> t -> int
  val params_of_t : t -> Params.t
  val on_collision : t -> t -> t option
end

module type MAP_GRID = sig
  type cell_type
  type t

  val empty : t
  val cell_type_ls : cell_type list

  val to_string : t -> string
  val of_alist_exn : (Coordinate.t * cell_type) list -> t
  val coordinate_set : t -> Coordinate.CoordinateSet.t
  val set : t -> key:Coordinate.t -> data:cell_type -> t
  val next : t -> width:int -> height:int -> t
end

module Make (CellType: CELL_TYPE) : MAP_GRID with type t = CellType.t Coordinate.CoordinateMap.t and type cell_type = CellType.t