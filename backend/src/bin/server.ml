open Core
open Dream
open Types
open Game
open Utils
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let () =
  run
  @@ logger
  @@ router
       [
         get "/" (fun _ -> html "Welcome to the Grid Game API!");
         post "/init_state" (fun request ->
             let%lwt body = body request in
             match Yojson.Safe.from_string body with
             | exception _ ->
                 respond ~status:`Bad_Request "Invalid JSON"
             | json -> (
                 try
                   let start =
                     Yojson.Safe.Util.(json |> member "start" |> to_list |> filter_int)
                   in
                   let width = Yojson.Safe.Util.(json |> member "width" |> to_int) in
                   let height = Yojson.Safe.Util.(json |> member "height" |> to_int) in
                   match start with
                   | [x; y] ->
                       let state = init_state (x, y) width height in
                       respond
                         ~headers:
                           [
                             ("Access-Control-Allow-Origin", "*");
                             ("Content-Type", "application/json");
                           ]
                         (Yojson.Safe.to_string (game_state_to_yojson state))
                   | _ -> respond ~status:`Bad_Request "Invalid start format"
                 with
                 | _ -> respond ~status:`Bad_Request "Invalid JSON format"))

         post "/next_state" (fun request ->
             let%lwt body = body request in
             match parse_body body with
             | Error msg -> respond ~status:`Bad_Request msg
             | Ok state -> (
                 match parse_move body with
                 | Error msg -> respond ~status:`Bad_Request msg
                 | Ok move ->
                     let next = next_state move state in
                     respond
                       ~headers:
                         [
                           ("Access-Control-Allow-Origin", "*");
                           ("Content-Type", "application/json");
                         ]
                       (Yojson.Safe.to_string (game_state_to_yojson next))))
       ]
