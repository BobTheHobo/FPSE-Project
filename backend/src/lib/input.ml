open Core

let example_cells = Simple_grid.Coordinate_set.of_list [
  { x = 0; y = 1 };
  { x = 0; y = 4 };
  { x = 2; y = 2 };
  { x = 2; y = 3 };
] 
let example_grid = { Simple_grid.cells = example_cells; width = 5; height = 5 }

let blinker_cells = Simple_grid.Coordinate_set.of_list [
  { x = 2; y = 1 };
  { x = 2; y = 2 };
  { x = 2; y = 3 };
] 
let blinker_grid = { Simple_grid.cells = blinker_cells; width = 5; height = 5 }

module GridVis = Grid.MakeGridVis(Grid.ConwayCellVis)

let rec input_loop (grid : Simple_grid.t) : unit =
  Out_channel.(flush stdout);
  let inp : string option = In_channel.(input_line stdin) in
  let result (grid : Simple_grid.t) : Simple_grid.t = match inp with
  | Some "w" -> print_string "going up\n";  grid
  | Some "n" -> 
    let next_grid = Simple_grid.next grid 
    in
    GridVis.draw_grid next_grid;
    next_grid;
  | Some "help" -> print_string "type n then hit enter for next state\n"; grid
  | Some x -> printf "%s isn't a valid input\n" x; grid
  | None -> print_string "nothing\n"; grid
  in
  input_loop (result grid)

let start_game : unit = 
  GridVis.draw_grid blinker_grid;
  input_loop (blinker_grid)