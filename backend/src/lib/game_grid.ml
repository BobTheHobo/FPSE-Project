open Core
open Types
open Utils

let in_bounds (x, y) width height =
  x >= 0 && x < width && y >= 0 && y < height

let neighbors (x, y) width height =
  let potential_neighbors =
    [
      (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1); (* Cardinal neighbors *)
      (x - 1, y - 1); (x - 1, y + 1); (x + 1, y - 1); (x + 1, y + 1); (* Diagonal neighbors *)
    ]
  in
  List.fold_left potential_neighbors ~init:CoordinateSet.empty ~f:(fun acc coord ->
      if in_bounds coord width height then CoordinateSet.add acc coord else acc)
      
let random_obstacles count width height exclusions =
  generate_random_coordinates count width height exclusions
