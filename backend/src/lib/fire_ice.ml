open Core

module Base_grid = Grid.Make(Base_cell)

module Make (Fire_cell: Grid.Cell) (Ice_cell: Grid.Cell) = struct
  module Fire_grid = Grid.Make(Fire_cell)
  module Ice_grid = Grid.Make(Ice_cell)
  type t = {
    fire_grid : Fire_grid.t;
    ice_grid : Ice_grid.t
  }

  (* let next ({ fire_grid; ice_grid } : t) : t =
    let fire_next = Fire_grid.next fire_grid in
    let ice_next = Ice_grid.next ice_grid in
    let neutralized = Set.inter fire_next.cells ice_next.cells in
    let new_fire_grid = Set.diff fire_next.cells neutralized  in
    let new_ice_grid = Set.diff ice_next.cells neutralized in
    { fire_grid = new_fire_grid; ice_grid = new_ice_grid } *)
end

type t = {
  fire_grid : Base_grid.t;
  ice_grid : Base_grid.t;
}

let fire_grid = Base_grid.create [ 
  { x = 2; y = 1 }; { x = 2; y = 2 }; { x = 2; y = 3 };
] ~width:10 ~height:10
  
let ice_grid = Base_grid.create [
  { x = 4; y = 1 }; { x = 4; y = 2 }; { x = 4; y = 3 };
] ~width:10 ~height:10

let next () =
  let next_fire = Base_grid.next fire_grid in
  let next_ice = Base_grid.next ice_grid in
  let neutralized = Set.inter next_fire.cells next_ice.cells in
  let new_fire_cells = Set.diff fire_grid.cells neutralized in
  let new_ice_cells = Set.diff ice_grid.cells neutralized in
  { fire_grid = { cells = new_fire_cells; height = 10; width = 10 }; ice_grid = { cells = new_ice_cells; height = 10; width = 10 }}
