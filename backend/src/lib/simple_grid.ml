open Core

type coordinate = { x : int; y : int } [@@deriving sexp, compare, equal]

module Coordinate_key = struct
    type t = coordinate [@@deriving sexp, compare, equal]
end

module Coordinate_map = Map.Make(Coordinate_key)
module Coordinate_set = Set.Make(Coordinate_key)

type t = {
    cells : Coordinate_set.t;
    width : int;
    height : int;
}
let empty = Coordinate_map.empty

let in_bounds ({ x; y } : coordinate) ~(width : int) ~(height : int) =
    x >= 0 && x < width && y >= 0 && y < height

let surrounding ({ x; y } : coordinate) : Coordinate_set.t = Coordinate_set.of_list [
    { x; y = (y + 1) }; { x = (x + 1); y = (y + 1) }; { x = (x - 1); y = (y + 1) }; (* bottom 3 *)
    { x = (x - 1); y }; { x = (x + 1); y }; (* left and right *)
    { x; y = (y - 1) }; { x = (x + 1); y = (y - 1) }; { x = (x - 1); y = (y - 1) } (* top 3 *)
]

let neighbors ({ x; y }: coordinate) ~(width : int) ~(height : int) : Coordinate_set.t =
    if not (in_bounds { x; y } ~width ~height) then Coordinate_set.empty
    else match x, y with
    | xtl, ytl when xtl = 0 && ytl = 0 -> Coordinate_set.of_list [
        { x = 1; y = 0 }; { x = 1; y = 1 }; { x = 0; y = 1 }
    ]
    | xtr, ytr when xtr = (width - 1) && ytr = 0 -> Coordinate_set.of_list [
        { x = (width - 2); y = 0 }; { x = (width - 2); y = 1 }; { x = (width - 1); y = 1 }
    ]
    | xbl, ybl when xbl = 0 && ybl = (height - 1) -> Coordinate_set.of_list [
        { x = 0; y = (height - 2) }; { x = 1; y = (height - 2) }; { x = 1; y = (height - 1) }
    ]
    | xbr, ybr when xbr = (width - 1) && ybr = (height - 1) -> Coordinate_set.of_list [
        { x = (width - 1); y = (height - 2) }; { x = (width - 2); y = (height - 2) }; { x = (width - 2); y = (height - 1) }
    ]
    | x, y -> surrounding { x; y }

let get_count (map : int Coordinate_map.t) (coordinate : coordinate) : int =
    match Map.find map coordinate with
    | None -> 0
    | Some count -> count;;

let get_count_map (map : int Coordinate_map.t) (neighbors : Coordinate_set.t) ~(alive : Coordinate_set.t) : int Coordinate_map.t  =
    Set.fold neighbors ~init:map ~f:(fun acc neighbor ->
        get_count acc neighbor
        |> (fun count ->
            if Set.mem alive neighbor then Map.set acc ~key:neighbor ~data:(count + 1)
            else acc
        )
    )

let to_count_map ({ cells; width; height } : t) : int Coordinate_map.t =
    Set.fold cells ~init:Coordinate_map.empty ~f:(fun acc coord ->
        let candidates = neighbors coord ~width ~height in
        get_count_map acc candidates ~alive:cells
    )

let alive_neighbors (cell : coordinate) ({ cells; width; height } : t) : Coordinate_set.t =
    neighbors cell ~width ~height
    |> Set.inter cells

let survive_set ({ cells; width; height } : t) : Coordinate_set.t =
    Set.filter cells ~f:(fun coordinate ->
        alive_neighbors coordinate { cells; width; height }
        |> fun alive_set -> 
            Set.length alive_set > 1 && Set.length alive_set < 4
    )

let spawn_set ({ cells; width; height  } : t) : Coordinate_set.t =
    to_count_map { cells; width; height }
    |> Map.filter ~f:(fun count -> count = 3)
    |> Map.fold ~init:Coordinate_set.empty ~f:(fun ~key ~data:_ acc ->
        Set.add acc key
    )

(** [next curr] is the new grid state given the curr state *)
let next ({ cells; width; height } : t) : t =
    let spawned = spawn_set { cells; width; height } in
    let survived = survive_set { cells; width; height } in
    { cells = Set.union spawned survived; width; height }
