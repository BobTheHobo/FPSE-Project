open Core
open OUnit2
open Game

module Cell_type : Map_grid.CELL_TYPE = struct
  module T = struct
    type t =
      | Fire 
      | Ice 
      | Water [@@deriving sexp, compare]
  end
  include T
  module TSet = Set.Make(T)
  
  let to_string (t : t) = Sexp.to_string (sexp_of_t t)
  let type_list = [T.Fire; T.Ice; T.Water] 
  let compare = T.compare
  let params_of_t (t : t) = match t with
    | Fire -> { Map_grid.Params.b = 3; s1 = 2; s2 = 3 }
    | Ice -> { b = 3; s1 = 2; s2 = 3 }
    | Water -> { b = 3; s1 = 2; s2 = 3 }
    
  let handle_collision (a : t) (b : t) = match (a, b) with
    | (Fire, Ice) -> Some Water
    | (Ice, Water) -> Some Ice
    | _ -> None
    
  let handle_collisions (tset : TSet.t) = match Set.to_list tset with
    | [] -> None
    | a :: [] -> Some a
    | a :: b :: [] -> handle_collision a b
    | a :: b :: c :: [] -> (
      match handle_collision a b with
      | None -> Some c
      | Some cell -> handle_collision cell c
    )
    | _ -> None
end

module M_grid = Map_grid.Make(Cell_type)

let create_coordinate ~x ~y = { Map_grid.Coordinate.T.x; y }
let get_fire = 
  match Cell_type.type_list with
  | fire_cell :: _ -> fire_cell
  | _ -> failwith "lol"

let get_ice = 
  match Cell_type.type_list with
  | _ :: ice_cell :: _ -> ice_cell
  | _ -> failwith "lol"

let get_water = 
  match Cell_type.type_list with
  | _ :: _ :: water_cell :: _ -> water_cell
  | _ -> failwith "lol"

let get_fire_set = Cell_type.TSet.of_list [get_fire]
  
let get_ice_set = Cell_type.TSet.of_list [get_ice]

let is_same_type (a : Cell_type.t) (b : Cell_type.t) = Cell_type.compare a b = 0
  
let get_water_set = Cell_type.TSet.of_list [get_water]

let test_all_dead_from_solitude_example _ =
  let coordinate_1 = { Map_grid.Coordinate.T.x = 0; y = 0 } in
  let coordinate_2 = { Map_grid.Coordinate.T.x = 2; y = 2 } in
  let fire_cell = List.hd_exn Cell_type.type_list in
  let set = Cell_type.TSet.of_list [fire_cell] in
  let m = Map.add_exn M_grid.empty ~key:coordinate_1 ~data:set in
  let grid_map = Map.add_exn m ~key:coordinate_2 ~data:set in
  let all_dead = M_grid.next grid_map ~width:4 ~height: 4 in
  assert_equal (Map.length all_dead) 0
  
let test_corners_alive_overpopulation_example _ =
  let alive_left = create_coordinate ~x:0 ~y:0 in
  let alive_right = create_coordinate ~x:3 ~y:0 in
  let c1 = create_coordinate ~x:1 ~y:0 in
  let c2 = create_coordinate ~x:2 ~y:0 in
  let c3 = create_coordinate ~x:2 ~y:1 in
  let c4 = create_coordinate ~x:1 ~y:1 in
  let m = M_grid.CMap.of_alist_exn [
    (alive_left, get_fire_set);
    (alive_right, get_fire_set);
    (c1, get_fire_set);
    (c2, get_fire_set);
    (c3, get_fire_set);
    (c4, get_fire_set);
  ] in
  let mnext = M_grid.next m ~width:4 ~height:4 in
  assert_bool "Map keys unexpected empty" ((Map.length mnext) > 0);
  assert_equal (Map.length mnext) 4;
  assert_equal ([
    (create_coordinate ~x:0 ~y:0);
    (create_coordinate ~x:0 ~y:1);
    (create_coordinate ~x:3 ~y:0);
    (create_coordinate ~x:3 ~y:1);
  ]) (Map.keys mnext)
  
let test_spawn_example _ =
  let c1 = create_coordinate ~x:0 ~y:0 in
  let c2 = create_coordinate ~x:1 ~y:1 in
  let c3 = create_coordinate ~x:1 ~y:0 in
  let m = M_grid.CMap.of_alist_exn [
    (c1, get_fire_set);
    (c2, get_fire_set);
    (c3, get_fire_set)
  ] in
  let mnext = M_grid.next m ~width:4 ~height:4 in
  assert_equal (Map.length mnext) 4;
  assert_equal ([
    c1;
    create_coordinate ~x:0 ~y:1;
    c3;
    c2;
  ]) (Map.keys mnext)
  
let test_fire_ice_collision_example _ =
  let fire_ice_set = Set.union get_fire_set get_ice_set in
  let c = create_coordinate ~x:0 ~y:0 in
  let m = M_grid.CMap.of_alist_exn [
    (c, fire_ice_set)
  ] in
  let handled = M_grid.handle_collisions m in
  assert_equal (Map.length handled) 1;
  let ls = 
    Map.find_exn handled c
    |> Set.to_list
  in
  assert_equal (List.length ls) 1;
  match ls with
  | hd :: _ -> (
    assert_bool "Water should equal water" (is_same_type hd get_water);
    assert_bool "Fire shouldn't equal water" (not (is_same_type hd get_fire));
    assert_bool "Ice shouldn't equal water" (not (is_same_type hd get_ice));
  )
  | _ -> failwith "lol"
  
let rand_grid_dimension = (Quickcheck.random_value ~seed:`Nondeterministic (Int.gen_incl 3 50))

let test_all_dead_from_solitude_quickcheck _ =
  printf "Got random grid_dimension %d\n" rand_grid_dimension;
  let width = rand_grid_dimension in
  let height = rand_grid_dimension in
  let solitude_pos_sequence = Sequence.unfold ~init:0 ~f:(fun current ->
    if current >= rand_grid_dimension then
      None
    else
      Some (current, current + 2)
  ) in
  let solitude_pos_list = Sequence.to_list solitude_pos_sequence in
  let m = List.fold solitude_pos_list ~init:(M_grid.empty) ~f:(fun acc pos ->
    Map.add_exn acc ~key:({ x = pos; y = pos }) ~data:(get_water_set)
  ) in
  let mnext = M_grid.next m ~height ~width in
  assert_equal (Map.length mnext) 0

let example_suite =
  "Example suite"
  >::: [
        "test all_dead_from_solitude example" >:: test_all_dead_from_solitude_example;
        "test corners_alive_overpopulation example" >:: test_corners_alive_overpopulation_example;
        "test test_spawn_example" >:: test_spawn_example;
        "test test_fire_ice_collision" >:: test_fire_ice_collision_example;
       ];;
    
let quickcheck_suite =
  "Quickcheck suite"
  >::: [
    "test all_dead_from_solitude quickcheck" >:: test_all_dead_from_solitude_quickcheck;
  ];;

let () = 
  run_test_tt_main example_suite;;
  run_test_tt_main quickcheck_suite
