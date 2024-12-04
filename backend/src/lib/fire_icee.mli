open Core

module Make : functor (Fire_cell : Grid.Cell) (Ice_cell : Grid.Cell) -> sig
  module Fire_grid : module type of Grid.Make(Fire_cell)
  module Ice_grid : module type of Grid.Make(Ice_cell)
  type t = {
    fire_grid : Fire_grid.t;
    ice_grid : Ice_grid.t;
  }
  
  val neutralize : Fire_grid.t -> Ice_grid.t -> t
  val next : t -> t
end