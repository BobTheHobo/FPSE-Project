open Core

module type Cell = sig
  type t = { x : int; y : int } [@@deriving sexp, compare, equal]
  val to_string : t -> string
  
  val b : int
  val s1 : int
  val s2 : int
end

module Make (Key : Cell) = struct
  module Coordinate_set = Set.Make(Key)

  type t = { cells : Coordinate_set.t; width : int; height : int }
  
  let empty = { cells = Coordinate_set.empty; width = -1; height = -1 }

  let coordinate_set_to_string (set : Coordinate_set.t) : string = Sexp.to_string (Coordinate_set.sexp_of_t set)

  (** [in_bounds cell ~height ~width] if and only if cell.x >= 0 && cell.x < width, and cell.y >= 0 && cell.y < height *)

  let in_bounds ({ x; y } : Coordinate_set.Elt.t) ~(width : int) ~(height : int) =
    x >= 0 && x < width && y >= 0 && y < height

  (** [surrounding cell] are simply the bottom 3, top 3, and left and right neighbors of cell without regards to the bounds *)
  let surrounding ({ x; y } : Coordinate_set.Elt.t) : Coordinate_set.t =
    Coordinate_set.of_list
      [
        { x; y = y + 1 };
        { x = x + 1; y = y + 1 };
        { x = x - 1; y = y + 1 };
        (* bottom 3 *)
        { x = x - 1; y };
        { x = x + 1; y };
        (* left and right *)
        { x; y = y - 1 };
        { x = x + 1; y = y - 1 };
        { x = x - 1; y = y - 1 } (* top 3 *);
      ]

  (** [neighbors cell ~width ~height] is the set of all cells that are neighbors of the given [cell] and 
      satisfies [in_bounds cell ~width ~height]. *)
  let neighbors ({ x; y } : Coordinate_set.Elt.t) ~(width : int) ~(height : int) : Coordinate_set.t =
    if not (in_bounds { x; y } ~width ~height) then Coordinate_set.empty
    else
      surrounding { x; y }
      |> Set.filter ~f:(fun coord -> in_bounds coord ~width ~height)

  (** [alive_neighbors cell curr] is the set of coordinates that are currently within +-1 in the x and or y direction
      of [cell] and is in [curr.cells] *)
  let alive_neighbors (cell : Coordinate_set.Elt.t) ({ cells; width; height } : t) : Coordinate_set.t =
    neighbors cell ~width ~height |> Set.inter cells

  (** [survive_set curr] is the set of all cells in the current grid that have survived *)
  let survive_set ({ cells; width; height } : t) : Coordinate_set.t =
    Set.filter cells ~f:(fun coordinate ->
      alive_neighbors coordinate { cells; width; height } 
      |> fun alive_set ->
        Set.length alive_set = Key.s1 || Set.length alive_set = Key.s2 
    )

  (** [spawn_set curr] is the coordinate set of the newly spawned cells *)
  let spawn_set ({ cells; width; height } : t) : Coordinate_set.t =
    Set.fold cells ~init:Coordinate_set.empty ~f:(fun acc coord ->
      let candidates = neighbors coord ~width ~height in
      Set.fold candidates ~init:acc ~f:(fun acc candidate ->
        let all_neighbors = neighbors candidate ~width ~height in
        let alive_neighbors = Set.inter all_neighbors cells in
        if Set.length alive_neighbors = Key.b then Set.add acc candidate else acc))

  (** [next curr] is the new grid state given the curr state *)
  let next ({ cells; width; height } : t) : t =
    let spawned = spawn_set { cells; width; height } in
    let survived = survive_set { cells; width; height } in
    { cells = Set.union spawned survived; width; height }
    
  let create (init : Key.t list) ~width ~height =
    let cells = Coordinate_set.of_list init in
    { cells; width; height }
end
