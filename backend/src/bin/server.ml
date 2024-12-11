open Core
open Game

let get_game_cookie request =
  match Dream.cookie request "game_id" with
  | Some game_id ->
      Dream.log "game id %s\n" game_id;
      game_id
  | None -> Supervisor.get_id_then_incr ()

let () =
  Dream.run @@ Dream.set_secret "secret" @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html "Welcome to the Game!");
         Dream.post "/game/new" (fun request ->
             let%lwt body = Dream.body request in
             body |> Yojson.Safe.from_string |> Supervisor.game_params_of_yojson
             |> Supervisor.init_game_state
             |> fun game_id ->
             Supervisor.get_encodeable_game_state game_id
             |> Enc.encode_game_response_body
             |> Dream.response
                  ~headers:
                    [
                      ("Access-Control-Allow-Origin", "http://localhost:5173");
                      ("Access-Control-Allow-Credentials", "true");
                      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                      ("Access-Control-Allow-Headers", "Content-Type");
                      ("Content-Type", "application/json");
                    ]
             |> fun response ->
             Dream.set_cookie response request ~secure:false "game_id" game_id;
             Lwt.return response);
         Dream.post "/game" (fun request ->
             let game_id = get_game_cookie request in
             let curr_state = Supervisor.get_game_state game_id in
             Dream.log "\nCurrent state for id %s\n%s\n" game_id
               (Statetbl.to_string curr_state);
             let%lwt body = Dream.body request in
             Dream.log "%s\n" body;
             let pos = Enc.parse_playser_positionj body in
             let encodeable_next_state =
               Supervisor.next_game_state game_id pos
             in
             let raw_next_state = Supervisor.get_game_state game_id in
             Dream.log "\nSuccesfully updated state for id %s\n%s\n" game_id
               (Statetbl.to_string raw_next_state);
             Enc.encode_game_response_body encodeable_next_state
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
