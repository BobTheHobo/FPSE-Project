open Core
open OUnit2

open Game

let loop (grid : Simple_grid.t) (iter_limit : int) ~f:(f: Simple_grid.t -> int -> unit) : unit = 
    let rec next (prev : Simple_grid.t) (n : int) =
        if n = iter_limit then ()
        else (
            f prev n;
            next (Simple_grid.next prev) (n + 1)
        )
    in 
    next grid 0

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

(* Tests a blinker which is an oscillating pattern, see link below for gif *)
(* https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#/media/File:Game_of_life_blinker.gif *)
let test_blinker _ = 
    let blinker_cells = Simple_grid.Coordinate_set.of_list [
      { x = 2; y = 1 };
      { x = 2; y = 2 };
      { x = 2; y = 3 };
    ] in
    let grid = { Simple_grid.cells = blinker_cells; width = 5; height = 5 } in

    loop grid 10 ~f:(fun prev n ->
        let str = (Sexp.to_string (Simple_grid.Coordinate_set.sexp_of_t prev.cells)) in
        (* printf "%d : %s" n str; *)
        if ((n mod 2) = 0) then 
            assert_equal "(((x 2)(y 1))((x 2)(y 2))((x 2)(y 3)))" str
        else 
            assert_equal "(((x 1)(y 2))((x 2)(y 2))((x 3)(y 2)))" str
    )



let suite =
    "suite" >:::
    [
        "test simple next" >:: test_simple_next;
        "test blinker 10 iterations" >:: test_blinker
    ];;

let () = 
    run_test_tt_main suite;;