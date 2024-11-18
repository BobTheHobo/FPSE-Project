open Core
open Dream
open Game
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type obstacle_object = { obstacles : (int * int) list } [@@deriving yojson]

let random_el set =
  let len = Set.length set in
  Set.nth set (Random.int len) |> function
  | Some el -> el
  | None -> failwith "lol"

let init_obstacles ~width ~height =
  let first = { Grid.Coordinate_key.x = Random.int width; y = Random.int height } in
  let surrounding = Grid.neighbors first ~width ~height in
  let second = random_el surrounding in
  let removed = Set.remove surrounding second in
  let third = random_el removed in
  [ first; second; third ]

let coordinate_to_pair_list (ls : Grid.Coordinate_key.t list) =
  List.map ls ~f:(fun { x; y } -> (x, y))

let pair_to_coordinate_list (pairs : (int * int) list) =
  List.map pairs ~f:(fun (x, y) -> { Grid.Coordinate_key.x; y })

let get_next_obstacles obstacles : Grid.Coordinate_key.t list =
  match obstacles with
  | [] -> init_obstacles ~width:10 ~height:10
  | hd ->
      let set = Grid.Coordinate_set.of_list hd in
      let updated =
        Grid.next { Grid.cells = set; width = 10; height = 10 }
      in
      Set.to_list updated.cells

let () =
  run @@ logger
  @@ router
       [
         get "/" (fun _ -> html "Welcome to the Game!");
         post "/get_obstacles" (fun request ->
             let%lwt body = body request in
             let obstacle_object =
               body |> Yojson.Safe.from_string |> obstacle_object_of_yojson
             in
             obstacle_object.obstacles |> pair_to_coordinate_list
             |> get_next_obstacles |> coordinate_to_pair_list
             |> List.map ~f:(fun (x, y) -> Printf.sprintf "[%d, %d]" x y)
             |> (fun a -> String.concat a ~sep:", ")
             |> Printf.sprintf "[%s]"
             |> respond
                  ~headers:
                    [
                      ("Access-Control-Allow-Origin", "*");
                      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                      ("Access-Control-Allow-Headers", "Content-Type");
                      ("Content-Type", "application/json");
                    ]);
         options "/get_obstacles" (fun _ ->
             respond
               ~headers:
                 [
                   ("Access-Control-Allow-Origin", "*");
                   ("Access-Control-Allow-Headers", "*");
                 ]
               "");
       ]
