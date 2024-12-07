open Core

module Params = struct
  type t = { b : int ; s1 : int ; s2 : int } [@@deriving sexp, compare]
end

module Coordinate = struct
  module T = struct
    type t = { x : int ; y : int } [@@deriving sexp, compare]
  end
  include T
  
  let to_string (t : t) = Sexp.to_string (sexp_of_t t)
  
  module CSet = Set.Make (T)
  
  type grid = { coordinates: CSet.t; width: int; height: int }
  
  (** [surrounding cell] are simply the bottom 3, top 3, and left and right neighbors of cell without regards to the bounds *)
  let surrounding ({ x; y } : t) : CSet.t = CSet.of_list [
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

  (** [in_bounds cell ~height ~width] if and only if cell.x >= 0 && cell.x < width, and cell.y >= 0 && cell.y < height *)
  let in_bounds ({ x; y } : t) ~(width : int) ~(height : int) =
    x >= 0 && x < width && y >= 0 && y < height
  
  (** [neighbors cell ~width ~height] is the set of all cells that are neighbors of the given [cell] and 
      satisfies [in_bounds cell ~width ~height]. *)
  let neighbors ({ x; y } : t) ~(width : int) ~(height : int) =
    if not (in_bounds { x; y } ~width ~height) then CSet.empty
    else
      surrounding { x; y }
      |> Set.filter ~f:(fun coord -> in_bounds coord ~width ~height)

  (** [alive_neighbors cell curr] is the set of coordinates that are currently within +-1 in the x and or y direction
      of [cell] and is in [curr.cells] *)
  let alive_neighbors (coordinate : t) ({ coordinates; width; height } : grid) : CSet.t =
    neighbors coordinate ~width ~height |> Set.inter coordinates

  (** [survive_set curr] is the set of all cells in the current grid that have survived *)
  let survive_set ({ coordinates; width; height } : grid) ~(s1 : int) ~(s2 : int) : CSet.t =
    Set.filter coordinates ~f:(fun coordinate ->
      alive_neighbors coordinate { coordinates; width; height }
      |> fun alive_set ->
        Set.length alive_set = s1 || Set.length alive_set = s2 
    )

  (** [spawn_set curr] is the coordinate set of the newly spawned cells *)
  let spawn_set ({ coordinates; width; height } : grid) ~(b : int) : CSet.t =
    Set.fold coordinates ~init:CSet.empty ~f:(fun acc coord ->
      let candidates = neighbors coord ~width ~height in
      Set.fold candidates ~init:acc ~f:(fun acc candidate ->
        let all_neighbors = neighbors candidate ~width ~height in
        let alive_neighbors = Set.inter all_neighbors coordinates in
        if Set.length alive_neighbors = b then Set.add acc candidate else acc))
end

module type CELL_TYPE = sig
  module T : sig
    type t [@@deriving sexp, compare]
  end
  include T

  module TSet : Set.S with type Elt.t := t
  val to_string : t -> string
  
  val type_list : t list
  val compare : t -> t -> int
  val params_of_t : t -> Params.t
  val handle_collisions : TSet.t -> t option
end

module Make (Cell_type : CELL_TYPE) = struct
  module CMap = Map.Make (Coordinate)

  (* probably also want to map cell types to their params either with payload or a map *)
  type t = Cell_type.TSet.t CMap.t

  let empty = CMap.empty
  
  let is_neighbor (a : Coordinate.t) (b : Coordinate.t) = abs (a.x - b.x) <= 1 && abs (a.y - b.y) <= 1
  
  let lookup_neighbors (m : t) (cell_type : Cell_type.t) (coordinate : Coordinate.t) : Coordinate.CSet.t =
    Map.fold m ~init:(Coordinate.CSet.empty) ~f:(fun ~key ~data acc -> 
      if not (is_neighbor coordinate key) then acc 
      else (
        data
        |> Set.filter ~f:(fun cell -> Cell_type.compare cell cell_type = 0)
        |> fun filtered -> if Set.length filtered = 1 then (Set.add acc key) else acc
      )
    )
    
  (* Do something like this to handle collisions *)
  let handle_collisions (m : t) : t  =
    Map.map m ~f:(fun data -> match Cell_type.handle_collisions data with
      | None -> None
      | Some cell -> Some cell
    )
    |> Map.fold ~init:(empty) ~f:(fun ~key ~data acc ->
        match data with
        | None -> acc
        | Some cell -> Map.set acc ~key ~data:(Set.add Cell_type.TSet.empty cell)
      )
    
  let coordinates_of_type (m : t) (cell : Cell_type.t) : Coordinate.CSet.t =
    m 
    |> Map.keys
    |> List.fold ~init:(Coordinate.CSet.empty) ~f:(fun acc coordinate ->
      lookup_neighbors m cell coordinate
      |> Set.union acc
    )
    
  let cell_types (m : t) : Cell_type.TSet.t =
    m
    |> Map.data
    |> List.fold ~init:(Cell_type.TSet.empty) ~f:(fun acc cells ->
        Set.fold cells ~init:(acc) ~f:(fun acc cell ->
          Set.add acc cell
        )
      )
    
  (** [next m] is the new map state given the current state m *)
  let next (m : t) ~(width : int) ~(height : int) =
    m
    |> cell_types
    |> Set.fold ~init:(CMap.empty) ~f:(fun acc cell ->
      let { Params.b; s1; s2 } = Cell_type.params_of_t cell in
      let coordinates = coordinates_of_type m cell in
      let spawned = Coordinate.spawn_set { coordinates; width; height } ~b in
      let survived = Coordinate.survive_set { coordinates; width; height } ~s1 ~s2 in
      let all = Set.union spawned survived in
      let keys = Set.to_list all in
      List.fold keys ~init:acc ~f:(fun acc key ->
        match Map.find acc key with
        | None -> Map.add_exn acc ~key ~data:(Set.add Cell_type.TSet.empty cell)
        | Some set -> Map.add_exn acc ~key ~data:(Set.add set cell)
      )
    )
    |> handle_collisions
end

(* Now, in your main use of the grid *)

module Cells_tmp = struct
  type t =
    | Fire
    | Ice
    | Water
    [@@deriving sexp, compare]
end

(* then after user input *)

(* module Cell_type : CELL_TYPE = struct
  module T = Cells_tmp
  include T
  module TSet = Set.Make (T)

  let params_of_t = (* use user input *)
  let handle_collisions = (* use user input *)
end

let () =
  let module Grid = Map_grid (Cell_type) in
  (* then use the module in the code that follows *)
  () *)