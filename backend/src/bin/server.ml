open Core
open Game
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type params_json = { b : int; s1 : int; s2 : int } [@@deriving yojson]
type position = { x : int; y : int } [@@deriving yojson]
type game_post_request_body = { player_position : position } [@@deriving yojson]

let position_to_coordinate (pos : position) : Map_grid.Coordinate.t =
  { Map_grid.Coordinate.T.x = pos.x; y = pos.y }

let get_game_cookie request =
  match Dream.cookie request "game_id" with
  | Some game_id ->
      Dream.log "game id %s\n" game_id;
      game_id
  | None -> Int.to_string (State.next_game_id ())

let coordinate_to_assoc (coordinate : Map_grid.Coordinate.t) =
  `Assoc [ ("x", `Int coordinate.x); ("y", `Int coordinate.y) ]

let encode_map_grid (map_grid : State.Game_grid.t) =
  let map_as_list =
    Map.fold map_grid ~init:[] ~f:(fun ~key ~data acc ->
        let cell_type_list =
          Set.to_list data |> List.map ~f:State.Cell_type.to_string
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

let encode_response_body (map_grid : State.Game_grid.t)
    (player : Map_grid.Coordinate.t) (is_dead : bool) : string =
  let json =
    `Assoc
      [
        ("obstacles", encode_map_grid map_grid);
        ("player", coordinate_to_assoc player);
        ("is_dead", `Bool is_dead);
      ]
  in
  Yojson.Safe.to_string json
  
let encode_new_game_response_body (game_id : string) =
  `Assoc
    [
      ("game_id", `String game_id)
    ]
  |> Yojson.Safe.to_string

let () =
  Dream.run @@ Dream.set_secret "secret" @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html "Welcome to the Game!");
         Dream.post "/game/new" (fun request ->
             let open State in
             let%lwt body = Dream.body request in
             let params =
               body |> Yojson.Safe.from_string
               |> Supervisor.game_params_of_yojson
             in
             let game_id = Supervisor.create_game params in
             let response_body = encode_new_game_response_body game_id in
             let response =
               Dream.response response_body
                 ~headers:
                   [
                     ("Access-Control-Allow-Origin", "http://localhost:5173");
                     ("Access-Control-Allow-Credentials", "true");
                     ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                     ("Access-Control-Allow-Headers", "Content-Type");
                     ("Content-Type", "application/json");
                   ]
             in
             Dream.set_cookie response request ~secure:false "game_id" game_id;
             Lwt.return response);
         Dream.post "/game" (fun request ->
             let game_id = get_game_cookie request in
             let game_id_encoded = Int.of_string game_id in
             Dream.log "Received game_id: %s\n" game_id;
             let%lwt body = Dream.body request in
             let { player_position } =
               body |> Yojson.Safe.from_string
               |> game_post_request_body_of_yojson
             in
             let player_coordinate = position_to_coordinate player_position in
             let game_state =
               match State.get_game_state game_id_encoded with
               | None -> State.new_game_state ~width:15 ~height:15
               | Some _ ->
                   State.next_game_state player_coordinate game_id_encoded
             in
             let is_dead =
               match Map.find game_state.obstacles player_coordinate with
               | None -> false
               | Some _ -> true
             in
             let str = State.game_grid_to_string game_state.obstacles in
             Dream.log "obstacles state %s\n" str;
             State.set_game_state game_id_encoded game_state;
             encode_response_body game_state.obstacles
               game_state.player_position is_dead
             |> fun body ->
             let response =
               Dream.response body
                 ~headers:
                   [
                     ("Access-Control-Allow-Origin", "http://localhost:5173");
                     ("Access-Control-Allow-Credentials", "true");
                     ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                     ("Access-Control-Allow-Headers", "Content-Type");
                     ("Content-Type", "application/json");
                   ]
             in
             Dream.set_cookie response request ~secure:false "game_id" game_id;
             Lwt.return response);
         Dream.options "/game" (fun _ ->
             Dream.respond
               ~headers:
                 [
                   ("Access-Control-Allow-Origin", "http://localhost:5173");
                   ("Access-Control-Allow-Headers", "Content-Type");
                   ("Access-Control-Allow-Credentials", "true");
                 ]
               "");
         Dream.options "/game/new" (fun _ ->
             Dream.respond
               ~headers:
                 [
                   ("Access-Control-Allow-Origin", "http://localhost:5173");
                   ("Access-Control-Allow-Headers", "Content-Type");
                   ("Access-Control-Allow-Credentials", "true");
                 ]
               "");
       ]
