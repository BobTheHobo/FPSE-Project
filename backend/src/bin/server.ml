open Core
open Dream
open Game
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Key = struct
  type t = { x : int; y : int } [@@deriving sexp, compare, equal]
  let to_string key = Sexp.to_string (sexp_of_t key)
  
  let b = 3
  let s1 = 2
  let s2 = 3
end

module Base_game = Grid.Make(Key)

type obstacle_object = { obstacles : (int * int) list; player : (int * int) } [@@deriving yojson]

let random_el set =
  let len = Set.length set in
  Set.nth set (Random.int len) |> function
  | Some el -> el
  | None -> failwith "lol"

let init_obstacles ~width ~height =
  let first = { Key.x = Random.int width; y = Random.int height } in
  let surrounding = Base_game.neighbors first ~width ~height in
  let second = random_el surrounding in
  let removed = Set.remove surrounding second in
  let third = random_el removed in
  [ first; second; third ]

let pair_to_coordinate ((x, y) : (int * int)) = { Key.x; y }

let coordinate_to_pair_list (ls : Key.t list) =
  List.map ls ~f:(fun { Key.x; y } -> (x, y))

let pair_to_coordinate_list (pairs : (int * int) list) =
  List.map pairs ~f:(fun (x, y) -> { Key.x ; y })

let get_next_obstacles (obstacles : Key.t list) (player : Key.t) : (Key.t list * bool) =
  match obstacles with
  | [] -> init_obstacles ~width:10 ~height:10
          |> fun ls -> (ls, false)
  | hd ->
      let set = Base_game.Coordinate_set.of_list hd in
      let { Base_game.cells; _ } =
        Base_game.next { Base_game.cells = set; width = 10; height = 10 }
      in let is_dead = (Set.mem cells player) in
      (Set.to_list cells, is_dead)

let () =
  run @@ logger
  @@ router
       [
         get "/" (fun _ -> html "Welcome to the Game!");
         post "/get_obstacles" (fun request ->
             let%lwt body = body request in
              Dream.log "body: %s\n" body;
             let obstacle_object =
               body |> Yojson.Safe.from_string |> obstacle_object_of_yojson
             in
             obstacle_object.obstacles
             |> pair_to_coordinate_list
             |> fun coordinate_ls -> get_next_obstacles coordinate_ls (pair_to_coordinate obstacle_object.player)
             |> fun (next_state, is_dead) -> coordinate_to_pair_list next_state
             |> List.map ~f:(fun (x, y) -> Printf.sprintf "[%d, %d]" x y)
             |> (fun a -> String.concat a ~sep:", ")
             |> fun obstacles_encoded -> Printf.sprintf "{ \"obstacles\": [%s], \"is_dead\": %s }" obstacles_encoded (Bool.to_string is_dead)
             |> fun encoded -> Dream.log "HERE %s\n" encoded; encoded
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
