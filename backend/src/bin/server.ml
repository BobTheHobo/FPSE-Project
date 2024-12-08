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


type position = { x : int; y : int } [@@deriving yojson]

type game_post_request_body = { game_id : int; player_position : position }
[@@deriving yojson]

let position_to_coordinate (pos : position) : Map_grid.Coordinate.t =
  { Map_grid.Coordinate.T.x = pos.x; y = pos.y }

let () =
  run @@ logger
  @@ router
       [
         get "/" (fun _ -> html "Welcome to the Game!");
         post "/game" (fun request ->
            let game_states = State.get_game_state_tbl () in
            Dream.log "Game states are %s\n" (State.game_state_tbl_to_string game_states);
             let%lwt body = body request in
             let { game_id; player_position } =
               body |> Yojson.Safe.from_string
               |> game_post_request_body_of_yojson
             in
             let (game_id, game_state) =
               if game_id = -1 then State.new_game ~width:15 ~height:15
               else 
                let player_coordinate = position_to_coordinate player_position in
                State.next_game_state game_id player_coordinate
             in
             Dream.log "Got back game_id %d\n" game_id;
             State.encode_response_body game_id game_state.obstacles
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
