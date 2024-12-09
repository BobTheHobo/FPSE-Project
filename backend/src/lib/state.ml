open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Cell_type : Map_grid.CELL_TYPE = struct
  module T = struct
    type t = Fire | Ice | Water [@@deriving sexp, compare]
  end

  include T
  module TSet = Set.Make (T)

  let to_string (t : t) = Sexp.to_string (sexp_of_t t)
  let type_list = [ T.Fire; T.Ice; T.Water ]
  let compare = T.compare

  let params_of_t (t : t) =
    match t with
    | Fire -> { Map_grid.Params.b = 3; s1 = 2; s2 = 3 }
    | Ice -> { b = 3; s1 = 2; s2 = 3 }
    | Water -> { b = 3; s1 = 2; s2 = 3 }

  let handle_collision (a : t) (b : t) =
    match (a, b) with
    | Fire, Ice -> Some Water
    | Ice, Water -> Some Ice
    | _ -> None

  let handle_collisions (tset : TSet.t) =
    match Set.to_list tset with
    | [] -> None
    | a :: [] -> Some a
    | [ a; b ] -> handle_collision a b
    | [ a; b; c ] -> (
        match handle_collision a b with
        | None -> Some c
        | Some cell -> handle_collision cell c)
    | _ -> None
end

module Game_grid = Map_grid.Make (Cell_type)

type game_state = {
  obstacles : Game_grid.t;
  player_position : Map_grid.Coordinate.t;
} [@@deriving sexp]

let game_state_tbl_to_string tbl =
  Hashtbl.sexp_of_t (fun key -> Int.sexp_of_t key) (fun state -> sexp_of_game_state state) tbl
  |> Sexp.to_string

let fire =
  match Cell_type.type_list with fire :: _ -> fire | _ -> failwith "lol"

let ice =
  match Cell_type.type_list with _ :: ice :: _ -> ice | _ -> failwith "lol"

let fire_set = Cell_type.TSet.of_list [ fire ]
let ice_set = Cell_type.TSet.of_list [ ice ]

let get_oscillating_pattern ({ x; y } : Map_grid.Coordinate.t) =
  [
    { Map_grid.Coordinate.T.x; y };
    { x = x + 2; y };
    { x = x + 2; y = y - 1 };
    { x = x + 4; y = y - 2 };
    { x = x + 4; y = y - 3 };
    { x = x + 4; y = y - 4 };
    { x = x + 6; y = y - 3 };
    { x = x + 6; y = y - 4 };
    { x = x + 6; y = y - 5 };
    { x = x + 7; y = y - 5 };
  ]

let max_game_count = 10
let game_state_tbl : (int, game_state) Hashtbl.t = Hashtbl.create (module Int) [@@deriving sexp]
let current_game_id = ref 0

let get_game_state game_id =
  Hashtbl.find game_state_tbl game_id
let set_game_state game_id new_state =
  Hashtbl.set game_state_tbl ~key:game_id ~data:new_state

let next_game_id () =
  let id = !current_game_id in
  current_game_id := (id + 1) mod 11;
  id
  
type position = { x : int; y : int } [@@deriving yojson]

let position_to_coordinate (pos : position) : Map_grid.Coordinate.t =
  { Map_grid.Coordinate.T.x = pos.x; y = pos.y }

let is_legal_move (last_pos : Map_grid.Coordinate.t)
    (curr_pos : Map_grid.Coordinate.t) =
  abs (last_pos.x - curr_pos.x) < 2 && abs (last_pos.y - curr_pos.y) < 2

let create_initial_obstacles ~(width : int) ~(height : int) =
  let fire_pos = get_oscillating_pattern { x = width / 2; y = height / 2 } in
  let ice_pos = get_oscillating_pattern { x = 0; y = height - 1 } in
  let mapped_to_fire = List.map fire_pos ~f:(fun c -> (c, fire_set)) in
  let mapped_to_ice = List.map ice_pos ~f:(fun c -> (c, ice_set)) in
  Game_grid.CMap.of_alist_exn (mapped_to_fire @ mapped_to_ice)
  
let get_game_state_tbl () = game_state_tbl

let err_message (game_id : int) = "No game with the given id "^(Int.to_string game_id)^" found"
      
let new_game_state ~width ~height = {
  player_position = { x = 0; y = 0 };
  obstacles = create_initial_obstacles ~width ~height
} 

let next_game_state next_position ~last_position ~obstacles_state =
  if not (is_legal_move next_position last_position) then new_game_state ~width:15 ~height:15
  else {
    obstacles = Game_grid.next obstacles_state ~width:15 ~height:15;
    player_position = next_position
  }

let coordinate_to_assoc (coordinate : Map_grid.Coordinate.t) =
  `Assoc [ ("x", `Int coordinate.x); ("y", `Int coordinate.y) ]

let encode_map_grid (map_grid : Game_grid.t) =
  let map_as_list =
    Map.fold map_grid ~init:[] ~f:(fun ~key ~data acc ->
        let cell_type_list =
          Set.to_list data |> List.map ~f:Cell_type.to_string
        in
        let entry =
          `Assoc
            [
              ("coordinate", coordinate_to_assoc key);
              ( "cell_types",
                `List (List.map cell_type_list ~f:(fun ct -> `String ct)) );
            ]
        in
        entry :: acc)
  in
  `List map_as_list

let encode_response_body (map_grid : Game_grid.t)
    (player : Map_grid.Coordinate.t) : string =
  let json =
    `Assoc
      [
        ("obstacles", encode_map_grid map_grid);
        ("player", coordinate_to_assoc player);
      ]
  in
  Yojson.Safe.to_string json