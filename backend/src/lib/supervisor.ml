open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type base_grid_map = Maker.T.t Coordinate.CoordinateMap.t

let max_count : int = 10
let game_id_count : int ref = ref 0

type grid_params = { b : int; s1 : int; s2 : int } [@@deriving yojson]

let encode_params ({ b; s1; s2 } : grid_params) = { Map_grid.Params.b; s1; s2 }

type game_params = {
  fire : grid_params;
  ice : grid_params;
  water : grid_params;
  width : int;
  height : int;
}
[@@deriving yojson]

let increment () =
  let curr = !game_id_count in
  game_id_count := curr + 1;
  curr

let string_of_id (id : int) = id |> Int.to_string
let has_available_slot () : bool = !game_id_count < 10
let get_id_then_incr () = increment () |> string_of_id

let get_id_safe () =
  if has_available_slot () then get_id_then_incr ()
  else failwith "No more games available :("

let random_start_position ~width ~height =
  let half_width, half_height = (width / 2, height / 2) in
  let w_offset = Random.int (half_width - 1) in
  let h_offset = Random.int (half_height - 1) in
  let op = match Random.int 2 with 1 -> ( + ) | _ -> ( - ) in
  { Coordinate.x = op half_width w_offset; y = op half_height h_offset }

let is_in_bounds (coordinate : Coordinate.t) ~height ~width =
  coordinate.x >= 0 && coordinate.x < width && coordinate.y >= 0
  && coordinate.y < height

let initial_obstacle_coordinates ~(width : int) ~(height : int) : Coordinate.t list =
  let start : Coordinate.t = random_start_position ~width ~height in
  let choice_1 : bool = Random.int 10 % 2 = 0 in
    let choice_2 : int = if Random.int 10 % 2 = 0 then 1 else -1 in
    let choice_3 : int = if Random.int 10 % 2 = 0 then 1 else -1 in
    start ::
    if choice_1 then
      [
        { Coordinate.x = start.x + 1; y = start.y };
        { Coordinate.x = start.x - 1; y = start.y };
        { Coordinate.x = start.x; y = start.y + choice_2 };
        { Coordinate.x = start.x + choice_3; y = start.y + (choice_2 * -1) }
      ]
    else
      [
        { Coordinate.x = start.x; y = start.y + 1 };
        { Coordinate.x = start.x; y = start.y - 1 };
        { Coordinate.x = start.x + choice_2; y = start.y };
        { Coordinate.x = start.x + (choice_2 * -1); y = start.y + choice_3 }
      ]

let get3 ls =
  match ls with
  | fire :: ice :: water :: _ -> [ fire; ice; water ]
  | _ -> failwith "HAHAHAHAHAHAHAHAHAHAHAAH"

let random_grid_start (module A : Map_grid.S with type t = base_grid_map) ~width
    ~height =
  List.fold A.cell_type_ls ~init:[] ~f:(fun acc cell_type ->
      let coordinates = initial_obstacle_coordinates ~width ~height in
      let mapped = List.map coordinates ~f:(fun c -> (c, cell_type)) in
      acc @ mapped)
  |> A.of_alist_exn

let before_after_str (state : Statetbl.t) =
  let module A = (val state.grid_module) in
  ( A.to_string state.obstacles,
    A.to_string (A.next state.obstacles ~width:state.width ~height:state.height)
  )

let init_game_state ({ fire; ice; water; width; height } : game_params) =
  let fire = encode_params fire in
  let ice = encode_params ice in
  let water = encode_params water in
  let params_of_t = Maker.make_params_of_t { fire; ice; water } in
  let module A = (val Maker.make_grid params_of_t) in
  let obstacles = random_grid_start (module A) ~width ~height in
  let game_id = get_id_safe () in
  Statetbl.set ~key:game_id
    {
      obstacles;
      grid_module = (module A);
      is_dead = false;
      player_position = { x = 0; y = 0 };
      width;
      height;
    };
  game_id

let get_game_state (id : string) : Statetbl.t =
  match Statetbl.get id with
  | Some v -> v
  | None -> failwith ("No game with id " ^ id ^ " could be found")

let get_encodeable_game_state (id : string) : Statetbl.encodeable_t =
  let v = get_game_state id in
  {
    obstacles = v.obstacles;
    height = v.height;
    width = v.width;
    player_position = v.player_position;
    is_dead = v.is_dead;
  }

let is_player_dead (player_position : Coordinate.t)
    (obstacle_coordinates : Coordinate.CoordinateSet.t) =
  Set.mem obstacle_coordinates player_position

let is_legal_move (prev : Coordinate.t) (next : Coordinate.t) =
  abs (prev.x - next.x) <= 1 && abs (prev.y - next.y) <= 1

let next_game_state (id : string) (next_position : Coordinate.t) :
    Statetbl.encodeable_t =
  let { Statetbl.obstacles; grid_module; width; height; player_position; _ } =
    get_game_state id
  in
  if not (is_legal_move player_position next_position) then
    failwith "Illegal move."
  else
    let (module A) = grid_module in
    let next_grid_state = A.next obstacles ~width ~height in
    let obs_coordinates = A.coordinate_set next_grid_state in
    let is_dead = is_player_dead next_position obs_coordinates in
    Statetbl.update ~id ~player_position:next_position
      ~obstacles:next_grid_state ~is_dead;
    { is_dead; player_position = next_position; obstacles; height; width }
