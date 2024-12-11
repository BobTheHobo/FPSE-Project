open Core
open OUnit2
open Game

module CellType : Map_grid.CELL_TYPE = struct
  module T = struct
    type t = Fire | Ice | Water [@@deriving sexp, compare]
  end

  include T
  module CellSet = Set.Make (T)

  let to_string (t : t) = Sexp.to_string (sexp_of_t t)
  let all_set = CellSet.of_list [ T.Fire; T.Ice; T.Water ]
  let cell_ls = [ T.Fire; T.Ice; T.Water ]
  let compare = T.compare

  let params_of_t (t : t) =
    match t with
    | Fire -> { Map_grid.Params.b = 3; s1 = 2; s2 = 3 }
    | Ice -> { b = 3; s1 = 2; s2 = 3 }
    | Water -> { b = 3; s1 = 2; s2 = 3 }

  let on_collision (a : t) (b : t) =
    if compare a b = 0 then Some a
    else
      match (a, b) with
      | Fire, Ice -> Some Water
      | Ice, Water -> Some Ice
      | _ -> None
end

module M_grid = Map_grid.Make (CellType)

let create_coordinate ~x ~y = { Coordinate.x; y }

let get_fire =
  match Set.to_list CellType.all_set with
  | fire_cell :: _ -> fire_cell
  | _ -> failwith "lol"

let get_ice =
  match Set.to_list CellType.all_set with
  | _ :: ice_cell :: _ -> ice_cell
  | _ -> failwith "lol"

let get_water =
  match Set.to_list CellType.all_set with
  | _ :: _ :: water_cell :: _ -> water_cell
  | _ -> failwith "lol"

let is_same_type (a : CellType.t) (b : CellType.t) = CellType.compare a b = 0

let test_all_dead_from_solitude_example _ =
  let coordinate_1 = { Coordinate.x = 0; y = 0 } in
  let coordinate_2 = { Coordinate.x = 2; y = 2 } in
  let fire_cell = List.hd_exn (Set.to_list CellType.all_set) in
  let m = Map.add_exn M_grid.empty ~key:coordinate_1 ~data:fire_cell in
  let grid_map = Map.add_exn m ~key:coordinate_2 ~data:fire_cell in
  let all_dead = M_grid.next grid_map ~width:4 ~height:4 in
  assert_equal (Map.length all_dead) 0

let test_corners_alive_overpopulation_example _ =
  let alive_left = create_coordinate ~x:0 ~y:0 in
  let alive_right = create_coordinate ~x:3 ~y:0 in
  let c1 = create_coordinate ~x:1 ~y:0 in
  let c2 = create_coordinate ~x:2 ~y:0 in
  let c3 = create_coordinate ~x:2 ~y:1 in
  let c4 = create_coordinate ~x:1 ~y:1 in
  let m =
    Coordinate.CoordinateMap.of_alist_exn
      [
        (alive_left, get_fire);
        (alive_right, get_fire);
        (c1, get_fire);
        (c2, get_fire);
        (c3, get_fire);
        (c4, get_fire);
      ]
  in
  let mnext = M_grid.next m ~width:4 ~height:4 in
  assert_bool "Map keys unexpected empty" (Map.length mnext > 0);
  assert_equal (Map.length mnext) 4;
  assert_equal
    [
      create_coordinate ~x:0 ~y:0;
      create_coordinate ~x:0 ~y:1;
      create_coordinate ~x:3 ~y:0;
      create_coordinate ~x:3 ~y:1;
    ]
    (Map.keys mnext)

let test_spawn_example _ =
  let c1 = create_coordinate ~x:0 ~y:0 in
  let c2 = create_coordinate ~x:1 ~y:1 in
  let c3 = create_coordinate ~x:1 ~y:0 in
  let m =
    Coordinate.CoordinateMap.of_alist_exn
      [ (c1, get_fire); (c2, get_fire); (c3, get_fire) ]
  in
  let mnext = M_grid.next m ~width:4 ~height:4 in
  assert_equal (Map.length mnext) 4;
  assert_equal [ c1; create_coordinate ~x:0 ~y:1; c3; c2 ] (Map.keys mnext)

let test_fire_ice_collision_example _ =
  match CellType.on_collision get_fire get_ice with
  | Some v -> assert_equal (is_same_type v get_water) true
  | None -> assert_failure "Unexpected none collision"

let rand_grid_dimension =
  Quickcheck.random_value ~seed:`Nondeterministic (Int.gen_incl 3 50)

let test_all_dead_from_solitude_quickcheck _ =
  printf "Got random grid_dimension %d\n" rand_grid_dimension;
  let width = rand_grid_dimension in
  let height = rand_grid_dimension in
  let solitude_pos_sequence =
    Sequence.unfold ~init:0 ~f:(fun current ->
        if current >= rand_grid_dimension then None
        else Some (current, current + 2))
  in
  let solitude_pos_list = Sequence.to_list solitude_pos_sequence in
  let m =
    List.fold solitude_pos_list ~init:M_grid.empty ~f:(fun acc pos ->
        Map.add_exn acc ~key:{ x = pos; y = pos } ~data:get_water)
  in
  let mnext = M_grid.next m ~height ~width in
  assert_equal (Map.length mnext) 0

let example_suite =
  "Example suite"
  >::: [
         "test all_dead_from_solitude example"
         >:: test_all_dead_from_solitude_example;
         "test corners_alive_overpopulation example"
         >:: test_corners_alive_overpopulation_example;
         "test test_spawn example" >:: test_spawn_example;
         "test test_fire_ice_collision example"
         >:: test_fire_ice_collision_example;
       ]

let quickcheck_suite =
  "Quickcheck suite"
  >::: [
         "test all_dead_from_solitude quickcheck"
         >:: test_all_dead_from_solitude_quickcheck;
       ]

let () = run_test_tt_main example_suite;;

run_test_tt_main quickcheck_suite
