open Core

type t = { x : int; y : int } [@@deriving sexp, compare]

module CoordinateSet : Set.S with type Elt.t = t
module CoordinateMap : Map.S with type Key.t = t

type grid = { coordinates : CoordinateSet.t; width : int; height : int }

val to_string : t -> string
val spawn_set : grid -> b:int -> CoordinateSet.t
val survive_set : grid -> s1:int -> s2:int -> CoordinateSet.t
val is_neighbor : t -> t -> bool
