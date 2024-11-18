open Core

module type Coordinate_key = sig
  type t = { x : int; y : int } [@@deriving sexp, compare, equal]
  val to_string : t -> string
end

module Make : functor (Key : Coordinate_key) -> sig
  module Coordinate_set : Set.S with type Elt.t = Key.t
  
  type t = { cells : Coordinate_set.t; width : int; height : int }
  
  val empty : t
  
  val next : t -> t
end