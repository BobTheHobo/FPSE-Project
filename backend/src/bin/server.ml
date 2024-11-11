open Core
open Dream
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type obstacle_object = {
  obstacles : (int * int) list
} [@@deriving yojson]


let update_obstacles obstacles : (int * int) list =
  let (x, y) : (int * int) = match List.last obstacles with
  | Some(move) -> move
  | None -> (5, 5)
  in 
  let next_move : (int * int) = 
    let choice : int = Random.int 3
    in 
    if choice = 0 then (x + 1, y)
    else if choice = 1 then (x, y + 1)
    else if choice = 2 then (x - 1, y)
    else (x, y - 1)
  in
  obstacles @ [next_move]

let () =
  run
  @@ logger
  @@ router [
       get "/" (fun _ -> html "Welcome to the Game!");
      post "/get_obstacles"
        (fun request ->
          let%lwt body = body request in
          let obstacle_object =
            body
            |> Yojson.Safe.from_string
            |> obstacle_object_of_yojson
          in
          update_obstacles obstacle_object.obstacles
          |> List.map ~f:(fun (x, y) -> Printf.sprintf "[%d, %d]" x y)
          |> (fun a -> String.concat a ~sep:", ")
          |> Printf.sprintf "[%s]"
          |> respond ~headers:[
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
            ("Access-Control-Allow-Headers", "Content-Type");
            ("Content-Type", "application/json")]);
        options "/get_obstacles" (fun _ -> respond ~headers:[
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Headers", "*")] "")
     ]
