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

(* let game_grid_to_string grid = 
  Game_grid.sexp_of_t grid
  |> Sexp.to_string

type game_state = {
  obstacles : Game_grid.t;
  player_position : Map_grid.Coordinate.t;
  height : int;
  width : int;
}
[@@deriving sexp] *)

(* let game_state_tbl_to_string tbl =
  Hashtbl.sexp_of_t
    (fun key -> Int.sexp_of_t key)
    (fun state -> sexp_of_game_state state)
    tbl
  |> Sexp.to_string *)

let fire =
  match Cell_type.type_list with fire :: _ -> fire | _ -> failwith "lol"

let ice =
  match Cell_type.type_list with _ :: ice :: _ -> ice | _ -> failwith "lol"
 
module ConfigTbl = struct
  type t = {
    width : int;
    height : int;
  }
  let tbl : (string, t) Hashtbl.t = Hashtbl.create (module String)
  let get (key : string) : t option = Hashtbl.find tbl key
  let delete (key : string) = Hashtbl.remove tbl key
  let set ~(key : string) (config : t) : unit = Hashtbl.set tbl ~key ~data:config
end
  
module StateTbl = struct
  open Map_grid
  type t = {
    obstacles : Game_grid.t;
    player_position : Coordinate.t;
    is_dead : bool;
  }
  let tbl : (string, t) Hashtbl.t = Hashtbl.create (module String)
  
  let to_string (t : t) =
    Printf.sprintf
    ("{\n"^^
      "  player_position: { x: %d, y: %d },\n"^^
      "  is_dead: %s,\n"^^
      "  obstacles: %s\n"^^
    "}")
    (t.player_position.x) (t.player_position.y)
    (Bool.to_string t.is_dead)
    (Game_grid.to_string t.obstacles)
  let get (key : string) = Hashtbl.find tbl key
  let delete (key : string) = Hashtbl.remove tbl key
  let set ~(key : string) (state : t)= Hashtbl.set tbl ~key ~data:state
end

module Pattern = struct
  let oscillating ({ x; y } : Map_grid.Coordinate.t) : Map_grid.Coordinate.t list = 
    let open Map_grid.Coordinate.T in
  [
    { x; y };
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
  
  let to_coordinate_map_alist (coordinates : Map_grid.Coordinate.t list) (k : 'a) = 
    List.map coordinates ~f:(fun coordinate -> (coordinate, k))
end
let fire_set = Cell_type.TSet.of_list [ fire ]
let ice_set = Cell_type.TSet.of_list [ ice ]


module Supervisor = struct
  let max_count : int = 10
  let game_id_count : int ref = ref 0
  
  type grid_params = {
    b : int;
    s1 : int;
    s2 : int;
  } [@@deriving yojson]

  type game_params = {
    fire : grid_params;
    ice : grid_params;
    water : grid_params;
    width : int;
    height : int;
  } [@@deriving yojson]
  
  let random_start_position ~width ~height =
    let half_width, half_height = (width / 2), (height / 2) in
    let open Map_grid.Coordinate.T in
    let w_offset = Random.int (half_width - 1) in
    let h_offset = Random.int (half_height - 1) in
    let op = (match Random.int 2 with
    | 1 -> (+)
    | _ -> (-)) in
    { x = (op half_width w_offset); y = (op half_height h_offset)}

  let increment () =
    let curr = !game_id_count in
    game_id_count := (curr + 1); 
    curr
    
  let string_of_id (id : int) = id |> Int.to_string
  let has_available_slot () : bool = !game_id_count < 10
  
  let get_id_then_incr () =
    increment ()
    |> string_of_id
    
  let initial_obstacles ~width ~height =
    [fire_set; ice_set]
    |> List.fold ~init:[] ~f:(fun acc singleton ->
      let start_pos = random_start_position ~width ~height in
      Pattern.oscillating start_pos
      |> fun coordinates -> Pattern.to_coordinate_map_alist coordinates singleton
      |> fun entries -> acc @ entries)
    |> List.filter ~f:(fun (coordinate, _) ->
      (coordinate.x > -1 && coordinate.x < width) 
      && (coordinate.y > -1 && coordinate.y < height)
    )
    |> List.fold ~init:(Game_grid.empty) ~f:(fun acc (coordinate, element_set) -> 
      (match Map.find acc coordinate with
      | Some set -> Set.union set element_set
      | None -> element_set)
      |> fun data -> Map.set acc ~key:coordinate ~data)
    
  let create_game ({ width; height; _ }: game_params) =
    if (has_available_slot ()) then
      let obstacles = initial_obstacles ~width ~height in
      let curr_id = get_id_then_incr () in
      ConfigTbl.set ~key:(curr_id) { width; height };
      StateTbl.set ~key:(curr_id) { obstacles; player_position = { x = 0; y = 0 }; is_dead = false };
      curr_id
    else Int.to_string (-1)
    
  let get_game_state (id : string) = match StateTbl.get id with
    | Some v -> v
    | None -> failwith ("No game with id "^id^" could be found")
    
  let get_game_config (id : string) = match ConfigTbl.get id with
    | Some v -> v
    | None -> failwith ("No game with id " ^ id ^ " could be found")
    
  let is_player_dead (player_position : Map_grid.Coordinate.t) (obstacle_coordinates : Map_grid.Coordinate.CSet.t) =
    Set.mem obstacle_coordinates player_position
    
  let next_game_state (id : string) (next_position : Map_grid.Coordinate.t) : StateTbl.t =
    (get_game_state id, get_game_config id)
    |> fun ({ StateTbl.obstacles; _}, { ConfigTbl.width; height }) ->
      let next_grid_state = Game_grid.next obstacles ~width:(width) ~height:(height) in
      let obs_coordinates = Game_grid.coordinate_set next_grid_state in
      let is_dead = is_player_dead next_position obs_coordinates in
      (next_grid_state, is_dead)
    |> fun (obstacles, is_dead) -> {
      StateTbl.is_dead = is_dead;
      player_position = next_position;
      obstacles = obstacles
    }
    
  let set_game_state (id : string) (new_state : StateTbl.t) =
    StateTbl.set ~key:id new_state
end

(*
let max_game_count = 10

let game_state_tbl : (int, game_state) Hashtbl.t = Hashtbl.create (module Int)
[@@deriving sexp]

let current_game_id = ref 0
let get_game_state game_id = Hashtbl.find game_state_tbl game_id
let get_game_state_exn game_id = Hashtbl.find_exn game_state_tbl game_id

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

let get_game_state_tbl () = game_state_tbl

let err_message (game_id : int) =
  "No game with the given id " ^ Int.to_string game_id ^ " found"

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

let create_initial_obstacles ~(width : int) ~(height : int) =
  let fire_pos = get_oscillating_pattern { x = width / 2; y = height / 2 } in
  let ice_pos = get_oscillating_pattern { x = 4; y = height - 2 } in
  let mapped_to_fire = List.map fire_pos ~f:(fun c -> (c, fire_set)) in
  let mapped_to_ice = List.map ice_pos ~f:(fun c -> (c, ice_set)) in
  Game_grid.CMap.of_alist_exn (mapped_to_fire @ mapped_to_ice) *)
(* let new_game_state ~width ~height =
  {
    player_position = { x = 0; y = 0 };
    obstacles = create_initial_obstacles ~width ~height;
    height;
    width;
  }

let next_game_state next_position game_id =
  let { player_position; obstacles; height; width } = get_game_state_exn game_id in
  if not (is_legal_move next_position player_position) then
    new_game_state ~width:15 ~height:15
  else
    let obstacles = Game_grid.next obstacles ~width ~height in
    {
      obstacles;
      player_position = next_position;
      width;
      height;
    } *)
