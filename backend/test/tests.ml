open Core
open OUnit2

open Game

let test_simple_next _ =
    let previous_cells = Simple_grid.Coordinate_set.of_list [
        { Simple_grid.x = 0; y = 1 };
        { x = 0; y = 4 };
        { x = 2; y = 2 };
        { x = 2; y = 3 };
    ] in
    let prev = { Simple_grid.cells = previous_cells; width = 5; height = 5 } in
    let next_grid = Simple_grid.next prev in
    printf "Prev cells is %s\n" (Sexp.to_string (Simple_grid.Coordinate_set.sexp_of_t prev.cells));
    printf "Next cells is %s\n" (Sexp.to_string (Simple_grid.Coordinate_set.sexp_of_t next_grid.cells));
;;

let suite =
    "suite" >:::
    ["test simple next" >:: test_simple_next];;

let () = 
    run_test_tt_main suite;;
