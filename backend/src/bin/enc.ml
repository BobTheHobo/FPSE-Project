open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Game

type coordinate_j = { x : int; y : int } [@@deriving yojson]
type player_position_j = { player_position : coordinate_j } [@@deriving yojson]
let encode_coordinate (coordinate : Coordinate.t) =
  `Assoc [ ("x", `Int coordinate.x); ("y", `Int coordinate.y) ]
let encode_map_grid (map_grid : Supervisor.base_grid_map) = 
let map_as_list =
  Map.fold map_grid ~init:[] ~f:(fun ~key ~data acc ->
      let entry =
        `Assoc
          [
            ("coordinate", encode_coordinate key);
            ("cell_type", `String (Maker.T.to_string data));
          ]
      in
      entry :: acc)
in
`List map_as_list
let encode_game_response_body (game_state : Statetbl.encodeable_t) =
`Assoc
  [
    ("obstacles", encode_map_grid game_state.obstacles);
    ("player_position", encode_coordinate game_state.player_position);
    ("is_dead", `Bool game_state.is_dead);
  ]
|> Yojson.Safe.to_string

let parse_playser_positionj (body : string) : Coordinate.t =
  body
  |> Yojson.Safe.from_string
  |> player_position_j_of_yojson
  |> fun { player_position = { x; y } } -> { Coordinate.x; y }