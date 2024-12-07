open Core
open Dream
open Game
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Key = struct
  type t = { x : int; y : int } [@@deriving sexp, compare, equal]

  let to_string key = Sexp.to_string (sexp_of_t key)
  
  let b = 3
  let s1 = 2
  let s2 = 3
end

module Base_game = Grid.Make (Key)

type obstacle_object = { fire : (int * int) list; ice : (int * int) list; player : (int * int) }
[@@deriving yojson]

type params_json = {
  b: int;
  s1: int;
  s2: int;
} [@@deriving yojson]

type game_params = {
  fire : params_json;
  ice : params_json;
  water : params_json;
} [@@deriving yojson]

let coordinate_to_pair_list (ls : Key.t list) : (int * int) list =
  List.map ls ~f:(fun { Key.x; y } -> (x, y))

let pair_to_coordinate_list (pairs : (int * int) list) : Key.t list=
  List.map pairs ~f:(fun (x, y) -> { Key.x; y })

let init_obstacles ~(width : int) ~(height : int) (is_fire : bool) : Base_game.Coordinate_set.t =
  let start : Key.t =
    let half_w = width/2 in
    let half_h = height/2 in
    if is_fire then { Key.x = Random.int (half_w - 2) + 1; y = Random.int (half_h - 2) + 1 + half_h }
    else { Key.x = Random.int (half_w - 2) + 1 + half_w; y = Random.int (half_h - 2) + 1 }
  in
  let choice_1 : bool = Random.int 10 % 2 = 0 in
  let choice_2 : int = if Random.int 10 % 2 = 0 then 1 else -1 in
  let choice_3 : int = if Random.int 10 % 2 = 0 then 1 else -1 in
  start
  ::
  (if choice_1 then
     [
       { Key.x = start.x + 1; y = start.y };
       { Key.x = start.x - 1; y = start.y };
       { Key.x = start.x; y = start.y + choice_2 };
       { Key.x = start.x + choice_3; y = start.y + (choice_2 * -1) };
     ]
   else
     [
       { Key.x = start.x; y = start.y + 1 };
       { Key.x = start.x; y = start.y - 1 };
       { Key.x = start.x + choice_2; y = start.y };
       { Key.x = start.x + (choice_2 * -1); y = start.y + choice_3 };
     ])
  |> Base_game.Coordinate_set.of_list

let get_next_state (prev_state : (int * int) list) (is_fire : bool) : Base_game.Coordinate_set.t =
  prev_state |> pair_to_coordinate_list
  |> (fun state ->
       match state with
       | [] -> init_obstacles ~width:10 ~height:10 is_fire
       | hd ->
           let set = Base_game.Coordinate_set.of_list hd in
           let updated =
             Base_game.next { Base_game.cells = set; width = 10; height = 10 }
           in
           updated.cells)
  
let diff (lhs : Base_game.Coordinate_set.t) (rhs : Base_game.Coordinate_set.t) : Base_game.Coordinate_set.t = Set.diff lhs rhs

let encode_pair ((x, y) : int * int) = Printf.sprintf "[%d, %d]" x y

let encode_set (coord_set : Base_game.Coordinate_set.t) : string =
  coord_set
  |> Set.to_list
  |> coordinate_to_pair_list
  |> List.map ~f:(encode_pair)
  |> fun a -> String.concat a ~sep:", "
  
let player_pos (is_dead : bool) (passed_pos : (int * int)) = if is_dead then (0, 0) else passed_pos

let get_next_board (fire_state : (int * int) list) (ice_state : (int * int) list) ((player_x, player_y): (int * int)): string =
  let next_fire_state = get_next_state fire_state true in
  let next_ice_state = get_next_state ice_state false in
  let final_fire_state = diff next_fire_state next_ice_state in
  let final_ice_state = diff next_ice_state next_fire_state in
  let is_dead = Set.mem (Set.union final_fire_state final_ice_state) { x = player_x; y = player_y }  in
  let fire_encoded = encode_set next_fire_state in
  let ice_encoded = encode_set next_ice_state in
  Printf.sprintf "{ \"fire\": [%s], \"ice\": [%s], \"player\": %s }" fire_encoded ice_encoded (encode_pair (player_pos is_dead (player_x, player_y)))
  
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

module Game_grid = Map_grid.Make(Cell_type)
let fire = match Cell_type.type_list with
| fire :: _ -> fire
| _ -> failwith "lol"

let ice = match Cell_type.type_list with
| _ :: ice :: _ -> ice
| _ -> failwith "lol"

let fire_set = Cell_type.TSet.of_list [fire]
let ice_set = Cell_type.TSet.of_list [ice]

let coordinate_to_assoc (coordinate : Map_grid.Coordinate.t) =
  `Assoc [
    ("x", `Int coordinate.x);
    ("y", `Int coordinate.y)
  ]
  
let encode_map_grid (map_grid : Game_grid.t) =
  let map_as_list =
    Map.fold map_grid ~init:[] ~f:(fun ~key ~data acc ->
      let cell_type_list = Set.to_list data |> List.map ~f:(Cell_type.to_string) in
      let entry = `Assoc [
        ("coordinate", coordinate_to_assoc key);
        ("cell_types", `List (List.map cell_type_list ~f:(fun ct -> `String ct)))
      ] in
      entry :: acc
    )
  in 
  `List map_as_list
  
let encode_response_body (game_id : int) (map_grid : Game_grid.t) (player : Map_grid.Coordinate.t) : string =
  let json = `Assoc [
    ("game_id", `Int game_id);
    ("obstacles", encode_map_grid map_grid);
    ("player", coordinate_to_assoc player);
  ] in
  Yojson.Safe.to_string json
  
type game_state = {
  obstacles: Game_grid.t;
  player_position: Map_grid.Coordinate.t;
}

let init_grid_map_state =
  let random_fire_cell_positions = [
    { Map_grid.Coordinate.x = 5; y = 4 };
    { Map_grid.Coordinate.x = 4; y = 5 };
    { Map_grid.Coordinate.x = 6; y = 5 };
  ] in
  let random_ice_cell_positions = [
    { Map_grid.Coordinate.x = 10; y = 9 };
    { Map_grid.Coordinate.x = 9; y = 10 };
    { Map_grid.Coordinate.x = 11; y = 10 };
  ] in
  let mapped_to_fire = List.map random_fire_cell_positions ~f:(fun c -> (c, fire_set)) in
  let mapped_to_ice = List.map random_ice_cell_positions ~f:(fun c -> (c, ice_set)) in
  Game_grid.CMap.of_alist_exn (mapped_to_fire @ mapped_to_ice)


let game_states : (int, game_state) Hashtbl.t = Hashtbl.create (module Int)
let current_game_id = ref 0
let next_game_id () =
  let id = !current_game_id in
  current_game_id := (id + 1) mod 11;
  id

let create_initial_game_state () : game_state = {
  obstacles = init_grid_map_state;
  player_position = { x = 0; y = 0 };
}
let get_game_state game_id = Hashtbl.find game_states game_id
let set_game_state game_id new_state = Hashtbl.set game_states ~key:game_id ~data:new_state

let create_new_game_state () =
  let game_id = next_game_id () in
  let new_state = create_initial_game_state () in
  set_game_state game_id new_state;
  (game_id, new_state)

type position = { x : int; y : int } [@@deriving yojson]
type game_post_request_body = {
  game_id : int;
  player_position : position;
} [@@deriving yojson]

let position_to_coordinate (pos : position) : Map_grid.Coordinate.t = {
  Map_grid.Coordinate.x = pos.x;
  y = pos.y;
}

let is_legal_move (last_pos : Map_grid.Coordinate.t) (curr_pos : Map_grid.Coordinate.t) =
  abs (last_pos.x - curr_pos.x) < 2 && abs (last_pos.y - curr_pos.y) < 2

let next_game_state game_id next_player_position = match get_game_state game_id with
  | None -> create_initial_game_state ()
  | Some { obstacles; player_position } ->
    if not (is_legal_move player_position next_player_position) then create_initial_game_state ()
    else {
      player_position = next_player_position;
      obstacles = Game_grid.next obstacles ~width:15 ~height:15
    }

let () =
  run 
  @@ logger
  @@ router
    [
      get "/" (fun _ -> html "Welcome to the Game!");
      post "/get_obstacles" (fun request ->
        let%lwt body = body request in
        let obstacle_object =
          body |> Yojson.Safe.from_string |> obstacle_object_of_yojson
        in
        get_next_board obstacle_object.fire obstacle_object.ice obstacle_object.player
        |> respond ~headers:
          [
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
            ("Access-Control-Allow-Headers", "Content-Type");
            ("Content-Type", "application/json");
          ]);
      options "/get_obstacles" (fun _ ->
        respond ~headers:
          [
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Headers", "*");
          ]
        "");
      post "/game" (fun request ->
        let%lwt body = body request in
        let { game_id; player_position  } = body |> Yojson.Safe.from_string |> game_post_request_body_of_yojson in
        let player_coordinate = position_to_coordinate player_position in
        let (game_id, game_state) =
          if game_id = (-1) then create_new_game_state ()
          else (game_id, next_game_state game_id player_coordinate)
        in encode_response_body game_id game_state.obstacles game_state.player_position
        |> respond ~headers:
          [
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
            ("Access-Control-Allow-Headers", "Content-Type");
            ("Content-Type", "application/json");
        ]);
      options "/game" (fun _ ->
        respond ~headers:
          [
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Headers", "*");
          ]
      "");
    ]
