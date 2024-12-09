open Core
open Game
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type params_json = { b : int; s1 : int; s2 : int } [@@deriving yojson]

type game_params = {
  fire : params_json;
  ice : params_json;
  water : params_json;
}
[@@deriving yojson]

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

let () =
  Dream.run @@ Dream.set_secret "secret" @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html "Welcome to the Game!");
         Dream.post "/game" (fun request ->
             let game_id = get_game_cookie request in
             let game_id_encoded = Int.of_string game_id in
             Dream.log "%s\n" game_id;
             let%lwt body = Dream.body request in
             let { player_position } =
               body |> Yojson.Safe.from_string
               |> game_post_request_body_of_yojson
             in
             let player_coordinate = position_to_coordinate player_position in
             let game_state = (
             match State.get_game_state game_id_encoded with
             | None -> State.new_game_state ~width:15 ~height:15
            | Some { player_position = last_position; obstacles } -> 
              State.next_game_state player_coordinate ~last_position ~obstacles_state:(obstacles))
            in
              State.set_game_state game_id_encoded game_state;
                 State.encode_response_body game_state.obstacles game_state.player_position
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
                 Dream.set_cookie response request ~secure:false "game_id"
                   game_id;
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
       ]
