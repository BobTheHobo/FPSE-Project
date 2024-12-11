open Core
open Game
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type params_json = { b : int; s1 : int; s2 : int } [@@deriving yojson]
type position = { x : int; y : int } [@@deriving yojson]
type game_post_request_body = { player_position : position } [@@deriving yojson]

let position_to_coordinate (pos : position) : Coordinate.t =
  { Coordinate.x = pos.x; y = pos.y }

let get_game_cookie request =
  match Dream.cookie request "game_id" with
  | Some game_id ->
      Dream.log "game id %s\n" game_id;
      game_id
  | None -> State.Supervisor.get_id_then_incr ()

let coordinate_to_assoc (coordinate : Coordinate.t) =
  `Assoc [ ("x", `Int coordinate.x); ("y", `Int coordinate.y) ]

let encode_map_grid (map_grid : State.base_grid_map) =
  let map_as_list =
    Map.fold map_grid ~init:[] ~f:(fun ~key ~data acc ->
        let entry =
          `Assoc
            [
              ("coordinate", coordinate_to_assoc key);
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
      ("player_position", coordinate_to_assoc game_state.player_position);
      ("is_dead", `Bool game_state.is_dead);
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
             let game_id = Supervisor.init_game_state params in
             let game_state = Supervisor.get_encodeable_game_state game_id in
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
             let curr_state = State.Supervisor.get_game_state game_id in
             Dream.log "\nCurrent state for id %s\n%s\n" game_id
               (Statetbl.to_string curr_state);
             let%lwt body = Dream.body request in
             let yojsoned = Yojson.Safe.from_string body in
             let a = game_post_request_body_of_yojson yojsoned in
             let pos = position_to_coordinate a.player_position in
             let encodeable_next_state =
               State.Supervisor.next_game_state game_id pos
             in
             let raw_next_state = State.Supervisor.get_game_state game_id in
             Dream.log "\nSuccesfully updated state for id %s\n%s\n" game_id
               (Statetbl.to_string raw_next_state);
             encode_game_response_body encodeable_next_state
             |> Dream.respond
                  ~headers:
                    [
                      ("Access-Control-Allow-Origin", "http://localhost:5173");
                      ("Access-Control-Allow-Credentials", "true");
                      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                      ("Access-Control-Allow-Headers", "Content-Type");
                      ("Content-Type", "application/json");
                    ]);
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
