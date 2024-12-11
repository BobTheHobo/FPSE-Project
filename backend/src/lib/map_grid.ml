open Core

module Params = struct
  [@@@coverage off]
  type t = { b : int; s1 : int; s2 : int } [@@deriving sexp, compare]
  [@@@coverage on]
end

module type CELL_TYPE = sig
  type t [@@deriving sexp, compare]
  module CellSet : Set.S with type Elt.t := t
  
  val all_set : CellSet.t 
  val to_string : t -> string
  val compare : t -> t -> int
  val params_of_t : t -> Params.t
  val on_collision : t -> t -> t option
end

module type MAP_GRID = sig
  type t [@@deriving sexp]
  val empty : t
  val to_string : t -> string
  val coordinate_set : t -> Coordinate.CoordinateSet.t
  val next : t -> width:int -> height:int -> t
end

module Make = functor (CellType : CELL_TYPE) -> struct
  type t = CellType.t Coordinate.CoordinateMap.t [@@deriving sexp]

  let empty = Coordinate.CoordinateMap.empty
  
  let assoc_to_string (({ x; y }, cell_type): Coordinate.t * CellType.t) : string =
    Printf.sprintf
    (
      "(%d, %d): [%s]"
    ) x y (CellType.to_string cell_type)

  let to_string (t : t) : string =
    Printf.sprintf
    (
      "{\n"^^"%s"^^"\n}"
    )
    (t
    |> Map.to_alist
    |> List.map ~f:(fun assoc -> "    " ^ assoc_to_string assoc)
    |> String.concat ~sep:"\n")

  (** [lookup_neighbors m cell_type coordinate] is the coordinate set of
    each neighbor determined by [coordinate] that are of type [cell_type] 
    in [m]
  *)
  let lookup_neighbors (m : t) (cell_type : CellType.t)
      (coordinate : Coordinate.t) : Coordinate.CoordinateSet.t =
    Map.fold m ~init:Coordinate.CoordinateSet.empty ~f:(fun ~key ~data acc ->
        if not (Coordinate.is_neighbor coordinate key) then acc
        else
          if (CellType.compare data cell_type = 0) then (Set.add acc key)
          else acc)

  (** [coordinates_of_type m cell] is the set [set] where
    [Set.every set ~f:(fun coordinate -> m[coordinate].type = cell)]
  *)
  let coordinates_of_type (m : t) (cell : CellType.t) : Coordinate.CoordinateSet.t =
    m |> Map.keys
    |> List.fold ~init:Coordinate.CoordinateSet.empty ~f:(fun acc coordinate ->
           lookup_neighbors m cell coordinate |> Set.union acc)
    
  let coordinate_set (m : t) : Coordinate.CoordinateSet.t =
    Map.keys m
    |> List.fold ~init:(Coordinate.CoordinateSet.empty) ~f:(fun acc coordinate -> Set.add acc coordinate)

  (** [cell_types m] is the Cell_type.TSet [set] of every present cell type in the map *)
  let cell_types (m : t) : CellType.CellSet.t =
    m |> Map.data
    |> List.fold ~init:CellType.CellSet.empty ~f:(fun acc cell -> Set.add acc cell)
    
  (** [next m] is the new map state given the current state m *)
  let next (m : t) ~(width : int) ~(height : int) =
    m
    |> cell_types
    |> Set.fold ~init:Coordinate.CoordinateMap.empty ~f:(fun acc cell ->
           let { Params.b; s1; s2 } = CellType.params_of_t cell in
           let coordinates = coordinates_of_type m cell in
           let spawned =
             Coordinate.spawn_set { coordinates; width; height } ~b
           in
           let survived =
             Coordinate.survive_set { coordinates; width; height } ~s1 ~s2
           in
           let all = Set.union spawned survived in
           let keys = Set.to_list all in
           List.fold keys ~init:acc ~f:(fun acc key ->
               match Map.find acc key with
               | None ->
                   Map.set acc ~key
                     ~data:(cell)
               | Some lhs -> 
                match CellType.on_collision lhs cell with
                | Some collided -> Map.set acc ~key ~data:(collided)
                | None -> acc))
end
