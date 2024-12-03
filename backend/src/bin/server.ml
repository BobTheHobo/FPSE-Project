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

module Base_game = Grid.Make (Key)

type obstacle_object = { fire : (int * int) list; ice : (int * int) list; player : (int * int) }
[@@deriving yojson]

let coordinate_to_pair_list (ls : Key.t list) : (int * int) list =
  List.map ls ~f:(fun { Key.x; y } -> (x, y))

let pair_to_coordinate_list (pairs : (int * int) list) : Key.t list=
  List.map pairs ~f:(fun (x, y) -> { Key.x; y })

let init_obstacles ~(width : int) ~(height : int) (is_fire : bool) : Base_game.Coordinate_set.t =
  let start : Key.t =
    let half_w = width/2 in
    let half_h = height/2 in
    if is_fire then { Key.x = Random.int (half_w - 2) + 1; y = Random.int (half_h - 2) + 1 + half_h }
    else { Key.x = Random.int (half_w - 2) + 1 + half_w; y = Random.int (half_h - 2) + 1 }
  in
  let choice_1 : bool = Random.int 10 % 2 = 0 in
  let choice_2 : int = if Random.int 10 % 2 = 0 then 1 else -1 in
  let choice_3 : int = if Random.int 10 % 2 = 0 then 1 else -1 in
  start
  ::
  (if choice_1 then
     [
       { Key.x = start.x + 1; y = start.y };
       { Key.x = start.x - 1; y = start.y };
       { Key.x = start.x; y = start.y + choice_2 };
       { Key.x = start.x + choice_3; y = start.y + (choice_2 * -1) };
     ]
   else
     [
       { Key.x = start.x; y = start.y + 1 };
       { Key.x = start.x; y = start.y - 1 };
       { Key.x = start.x + choice_2; y = start.y };
       { Key.x = start.x + (choice_2 * -1); y = start.y + choice_3 };
     ])
  |> Base_game.Coordinate_set.of_list

let get_next_state (prev_state : (int * int) list) (is_fire : bool) : Base_game.Coordinate_set.t =
  prev_state |> pair_to_coordinate_list
  |> (fun state ->
       match state with
       | [] -> init_obstacles ~width:10 ~height:10 is_fire
       | hd ->
           let set = Base_game.Coordinate_set.of_list hd in
           let updated =
             Base_game.next { Base_game.cells = set; width = 10; height = 10 }
           in
           updated.cells)
  
let diff (lhs : Base_game.Coordinate_set.t) (rhs : Base_game.Coordinate_set.t) : Base_game.Coordinate_set.t = Set.diff lhs rhs

let encode_pair ((x, y) : int * int) = Printf.sprintf "[%d, %d]" x y

let encode_set (coord_set : Base_game.Coordinate_set.t) : string =
  coord_set
  |> Set.to_list
  |> coordinate_to_pair_list
  |> List.map ~f:(encode_pair)
  |> fun a -> String.concat a ~sep:", "
  
let player_pos (is_dead : bool) (passed_pos : (int * int)) = if is_dead then (0, 0) else passed_pos

let get_next_board (fire_state : (int * int) list) (ice_state : (int * int) list) ((player_x, player_y): (int * int)): string =
  let next_fire_state = get_next_state fire_state true in
  let next_ice_state = get_next_state ice_state false in
  let final_fire_state = diff next_fire_state next_ice_state in
  let final_ice_state = diff next_ice_state next_fire_state in
  let is_dead = Set.mem (Set.union final_fire_state final_ice_state) { x = player_x; y = player_y }  in
  let fire_encoded = encode_set next_fire_state in
  let ice_encoded = encode_set next_ice_state in
  Printf.sprintf "{ \"fire\": [%s], \"ice\": [%s], \"player\": %s }" fire_encoded ice_encoded (encode_pair (player_pos is_dead (player_x, player_y)))

let () =
  run 
  @@ logger
  @@ router
    [
      get "/" (fun _ -> html "Welcome to the Game!");
      post "/get_obstacles" (fun request ->
        let%lwt body = body request in
        let obstacle_object =
          body |> Yojson.Safe.from_string |> obstacle_object_of_yojson
        in
        get_next_board obstacle_object.fire obstacle_object.ice obstacle_object.player
        |> respond ~headers:
          [
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
            ("Access-Control-Allow-Headers", "Content-Type");
            ("Content-Type", "application/json");
          ]);
      options "/get_obstacles" (fun _ ->
        respond ~headers:
          [
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Headers", "*");
          ]
        "");
    ]
