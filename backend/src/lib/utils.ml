open Core
open Types

let generate_random_coordinates n width height exclusions =
  let rec aux acc remaining =
    if remaining = 0 then acc
    else
      let candidate = (Random.int width, Random.int height) in
      if CoordinateSet.mem acc candidate || CoordinateSet.mem exclusions candidate
      then aux acc remaining
      else aux (CoordinateSet.add acc candidate) (remaining - 1)
  in
  aux CoordinateSet.empty n

let set_intersection set1 set2 =
  CoordinateSet.inter set1 set2

let set_difference set1 set2 =
  CoordinateSet.diff set1 set2

let is_within_bounds (x, y) width height =
  x >= 0 && x < width && y >= 0 && y < height
  
let parse_body body =
  try Ok (Yojson.Safe.from_string body |> Types.game_state_of_yojson) with
  | _ -> Error "Invalid JSON"
  

let parse_move body =
  try
    let json = Yojson.Safe.from_string body in
    match Yojson.Safe.Util.(member "move" json |> to_list |> filter_int) with
    | [dx; dy] -> Ok (dx, dy)
    | _ -> Error "Invalid move format"
  with
  | _ -> Error "Invalid JSON"
  
