open Core
open Dream
open Game
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type params_json = { b : int; s1 : int; s2 : int } [@@deriving yojson]

type game_params = {
  fire : params_json;
  ice : params_json;
  water : params_json;
}
[@@deriving yojson]

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

let fire =
  match Cell_type.type_list with fire :: _ -> fire | _ -> failwith "lol"

let ice =
  match Cell_type.type_list with _ :: ice :: _ -> ice | _ -> failwith "lol"

let fire_set = Cell_type.TSet.of_list [ fire ]
let ice_set = Cell_type.TSet.of_list [ ice ]

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

let encode_response_body (game_id : int) (map_grid : Game_grid.t)
    (player : Map_grid.Coordinate.t) : string =
  let json =
    `Assoc
      [
        ("game_id", `Int game_id);
        ("obstacles", encode_map_grid map_grid);
        ("player", coordinate_to_assoc player);
      ]
  in
  Yojson.Safe.to_string json

type game_state = {
  obstacles : Game_grid.t;
  player_position : Map_grid.Coordinate.t;
}

let init_grid_map_state =
  let random_fire_cell_positions =
    [
      { Map_grid.Coordinate.T.x = 5; y = 4 }; { x = 4; y = 5 }; { x = 6; y = 5 };
    ]
  in
  let random_ice_cell_positions =
    [
      { Map_grid.Coordinate.T.x = 10; y = 9 };
      { x = 9; y = 10 };
      { x = 11; y = 10 };
    ]
  in
  let mapped_to_fire =
    List.map random_fire_cell_positions ~f:(fun c -> (c, fire_set))
  in
  let mapped_to_ice =
    List.map random_ice_cell_positions ~f:(fun c -> (c, ice_set))
  in
  Game_grid.CMap.of_alist_exn (mapped_to_fire @ mapped_to_ice)

let game_states : (int, game_state) Hashtbl.t = Hashtbl.create (module Int)
let current_game_id = ref 0

let next_game_id () =
  let id = !current_game_id in
  current_game_id := (id + 1) mod 11;
  id

let create_initial_game_state () : game_state =
  { obstacles = init_grid_map_state; player_position = { x = 0; y = 0 } }

let get_game_state game_id = Hashtbl.find game_states game_id

let set_game_state game_id new_state =
  Hashtbl.set game_states ~key:game_id ~data:new_state

let create_new_game_state () =
  let game_id = next_game_id () in
  let new_state = create_initial_game_state () in
  set_game_state game_id new_state;
  (game_id, new_state)

type position = { x : int; y : int } [@@deriving yojson]

type game_post_request_body = { game_id : int; player_position : position }
[@@deriving yojson]

let position_to_coordinate (pos : position) : Map_grid.Coordinate.t =
  { Map_grid.Coordinate.T.x = pos.x; y = pos.y }

let is_legal_move (last_pos : Map_grid.Coordinate.t)
    (curr_pos : Map_grid.Coordinate.t) =
  abs (last_pos.x - curr_pos.x) < 2 && abs (last_pos.y - curr_pos.y) < 2

let next_game_state game_id next_player_position =
  match get_game_state game_id with
  | None -> create_initial_game_state ()
  | Some { obstacles; player_position } ->
      if not (is_legal_move player_position next_player_position) then
        create_initial_game_state ()
      else
        {
          player_position = next_player_position;
          obstacles = Game_grid.next obstacles ~width:15 ~height:15;
        }

let () =
  run @@ logger
  @@ router
       [
         get "/" (fun _ -> html "Welcome to the Game!");
         post "/game" (fun request ->
             let%lwt body = body request in
             let { game_id; player_position } =
               body |> Yojson.Safe.from_string
               |> game_post_request_body_of_yojson
             in
             let player_coordinate = position_to_coordinate player_position in
             let game_id, game_state =
               if game_id = -1 then create_new_game_state ()
               else (game_id, next_game_state game_id player_coordinate)
             in
             encode_response_body game_id game_state.obstacles
               game_state.player_position
             |> respond
                  ~headers:
                    [
                      ("Access-Control-Allow-Origin", "*");
                      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                      ("Access-Control-Allow-Headers", "Content-Type");
                      ("Content-Type", "application/json");
                    ]);
         options "/game" (fun _ ->
             respond
               ~headers:
                 [
                   ("Access-Control-Allow-Origin", "*");
                   ("Access-Control-Allow-Headers", "*");
                 ]
               "");
       ]
