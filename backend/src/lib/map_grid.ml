open Core

module Params = struct
  [@@@coverage off]
  type t = { b : int; s1 : int; s2 : int } [@@deriving sexp, compare]
  [@@@coverage on]
end

module type CELL_TYPE = sig
  module T : sig
    type t [@@deriving sexp, compare]
  end

  include T
  module TSet : Set.S with type Elt.t := t

  val type_list : t list
  (** [type_list] is the unique list of each variant type so as to make OCaml's compiler stop screaming at you for trying to make a cell type of t (lol) *)

  val to_string : t -> string
  val compare : t -> t -> int

  val params_of_t : t -> Params.t
  (** [params_of_t t] is the { b; s1; s2 } Conway game encoding for a cell type *)

  val handle_collisions : TSet.t -> t option
  (** [handle_collisions tset] is [Some t] if you want to encode an interaction between two overlapping cell types. Otherwise it is [None]*)
end

module type MAP_GRID = sig
  type 'a t = 'a Coordinate.CoordinateMap.t [@@deriving sexp]
  val empty : 'a t
  val to_string : 'a t -> string
  val handle_collisions : 'a t -> 'a t
  val coordinate_set : 'a t -> Coordinate.CoordinateSet.t
  val next : 'a t -> width:int -> height:int -> 'a t
end

module Make = functor (CellType : CELL_TYPE) -> struct
  type t = CellType.TSet.t Coordinate.CoordinateMap.t [@@deriving sexp]

  let empty = Coordinate.CoordinateMap.empty
  
  let assoc_to_string (({ x; y }, cell_set): Coordinate.t * CellType.TSet.t) : string =
    Printf.sprintf
    (
      "(%d, %d): [%s]"
    ) x y (Sexp.to_string (CellType.TSet.sexp_of_t cell_set))

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
          data
          |> Set.filter ~f:(fun cell -> CellType.compare cell cell_type = 0)
          |> fun filtered ->
          if Set.length filtered = 1 then Set.add acc key else acc)

  (** [handle_collisions m] is the final state of the grid where each coordinate maps to a Cell_type.TSet [set] where [Set.length set] <= 1 *)
  let handle_collisions (m : t) : t =
    Map.map m ~f:(fun data ->
        match CellType.handle_collisions data with
        | None -> None
        | Some cell -> Some cell)
    |> Map.fold ~init:empty ~f:(fun ~key ~data acc ->
           match data with
           | None -> acc
           | Some cell ->
               Map.set acc ~key ~data:(Set.add CellType.TSet.empty cell))

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
  let cell_types (m : t) : CellType.TSet.t =
    m |> Map.data
    |> List.fold ~init:CellType.TSet.empty ~f:(fun acc cells ->
           Set.fold cells ~init:acc ~f:(fun acc cell -> Set.add acc cell))

  (** [next m] is the new map state given the current state m *)
  let next (m : t) ~(width : int) ~(height : int) =
    m |> cell_types
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
                   Map.add_exn acc ~key
                     ~data:(Set.add CellType.TSet.empty cell)
               | Some set -> Map.set acc ~key ~data:(Set.add set cell)))
    |> handle_collisions
end
