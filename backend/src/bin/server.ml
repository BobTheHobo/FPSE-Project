open Core
open Dream
open Game
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Key = struct
  type t = { x : int; y : int } [@@deriving sexp, compare, equal]
  let to_string key = Sexp.to_string (sexp_of_t key)
end

module Base_game = Grid.Make(Key)

type obstacle_object = { obstacles : (int * int) list } [@@deriving yojson]


let init_obstacles ~(width : int) ~(height : int) : Key.t list = 
  let start : Key.t = { Key.x = Random.int (width-6) + 3; y = Random.int (height-6) + 3 } in
  let choice_1 : bool = Random.int 10 % 2 = 0 in
  let choice_2 : int = if Random.int 10 % 2 = 0 then 1 else -1 in
  let choice_3 : int = if Random.int 10 % 2 = 0 then 1 else -1 in
  start ::
  if choice_1 then
    [
      { Key.x = start.x + 1; y = start.y };
      { Key.x = start.x - 1; y = start.y };
      { Key.x = start.x; y = start.y + choice_2 };
      { Key.x = start.x + choice_3; y = start.y + (choice_2 * -1) }
    ]
  else
    [
      { Key.x = start.x; y = start.y + 1 };
      { Key.x = start.x; y = start.y - 1 };
      { Key.x = start.x + choice_2; y = start.y };
      { Key.x = start.x + (choice_2 * -1); y = start.y + choice_3 }
    ]

let coordinate_to_pair_list (ls : Key.t list) =
  List.map ls ~f:(fun { Key.x; y } -> (x, y))

let pair_to_coordinate_list (pairs : (int * int) list) =
  List.map pairs ~f:(fun (x, y) -> { Key.x ; y })

let get_next_obstacles obstacles : Key.t list =
  match obstacles with
  | [] -> init_obstacles ~width:10 ~height:10
  | hd ->
      let set = Base_game.Coordinate_set.of_list hd in
      let updated =
        Base_game.next { Base_game.cells = set; width = 10; height = 10 }
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
