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
  | None -> State.Supervisor.get_id_then_incr ()

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
              ("cell_types",
                `List (List.map cell_type_list ~f:(fun ct -> `String ct)) );
            ]
        in
        entry :: acc)
  in
  `List map_as_list
  
let encode_game_response_body (game_state : State.StateTbl.t) =
  `Assoc [
    ("obstacles", encode_map_grid game_state.obstacles);
    ("player_position", coordinate_to_assoc game_state.player_position);
    ("is_dead", `Bool game_state.is_dead)
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
             let game_state = Supervisor.get_game_state game_id in
             let response_body = encode_game_response_body game_state in
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
             let game_state = State.Supervisor.get_game_state game_id in
             let game_state_str = State.StateTbl.to_string game_state in
             Dream.log "State for id %s: %s" game_id game_state_str;
             let%lwt body = Dream.body request in
             let { player_position } =
               body |> Yojson.Safe.from_string
               |> game_post_request_body_of_yojson
             in
             let pos = position_to_coordinate player_position in
             let next_game_state = State.Supervisor.next_game_state game_id pos in
             let next_game_state_str = State.StateTbl.to_string next_game_state in
             Dream.log "Computed next state %s" next_game_state_str;
             encode_game_response_body next_game_state
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
