open Core

module T = struct
  type t = { x : int; y : int } [@@deriving sexp, compare]
end
include T

module CoordinateSet = Set.Make (T)
module CoordinateMap = Map.Make (T)

[@@@coverage off]
let to_string (t : t) = Sexp.to_string (sexp_of_t t)
[@@@coverage on]

type grid = { coordinates : CoordinateSet.t; width : int; height : int }

(** [surrounding cell] are simply the bottom 3, top 3, and left and right neighbors of cell without regards to the bounds *)
let surrounding ({ x; y } : t) : CoordinateSet.t =
  CoordinateSet.of_list
    [
      (* bottom 3 *)
      { x; y = y + 1 };
      { x = x + 1; y = y + 1 };
      { x = x - 1; y = y + 1 };
      (* left and right *)
      { x = x - 1; y };
      { x = x + 1; y };
      (* top 3 *)
      { x; y = y - 1 };
      { x = x + 1; y = y - 1 };
      { x = x - 1; y = y - 1 };
    ]

let is_neighbor (a : t) (b : t) = abs (a.x - b.x) <= 1 && abs (a.y - b.y) <= 1

(** [in_bounds cell ~height ~width] if and only if cell.x >= 0 && cell.x < width, and cell.y >= 0 && cell.y < height *)
let in_bounds ({ x; y } : t) ~(width : int) ~(height : int) =
  x >= 0 && x < width && y >= 0 && y < height

(** [neighbors cell ~width ~height] is the set of all cells that are neighbors of the given [cell] and 
    satisfies [in_bounds cell ~width ~height]. *)
let neighbors ({ x; y } : t) ~(width : int) ~(height : int) =
  if not (in_bounds { x; y } ~width ~height) then CoordinateSet.empty
  else
    surrounding { x; y }
    |> Set.filter ~f:(fun coord -> in_bounds coord ~width ~height)

(** [alive_neighbors cell grid] is the set of coordinates that are currently within +-1 in the x and or y direction
    of [cell] and is in [grid] *)
let alive_neighbors (coordinate : t) ({ coordinates; width; height } : grid) :
    CoordinateSet.t =
  neighbors coordinate ~width ~height |> Set.inter coordinates

(** [survive_set curr] is the set of all cells in the current grid that have survived *)
let survive_set ({ coordinates; width; height } : grid) ~(s1 : int)
    ~(s2 : int) : CoordinateSet.t =
  Set.filter coordinates ~f:(fun coordinate ->
      alive_neighbors coordinate { coordinates; width; height }
      |> fun alive_set ->
      Set.length alive_set = s1 || Set.length alive_set = s2)

(** [spawn_set curr] is the coordinate set of the newly spawned cells *)
let spawn_set ({ coordinates; width; height } : grid) ~(b : int) : CoordinateSet.t =
  Set.fold coordinates ~init:CoordinateSet.empty ~f:(fun acc coord ->
      let candidates = neighbors coord ~width ~height in
      Set.fold candidates ~init:acc ~f:(fun acc candidate ->
          let all_neighbors = neighbors candidate ~width ~height in
          let alive_neighbors = Set.inter all_neighbors coordinates in
          if Set.length alive_neighbors = b then Set.add acc candidate
          else acc))