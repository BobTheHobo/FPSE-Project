open Core

type coordinate = { x : int; y : int } [@@deriving sexp, compare, equal]

module Coordinate_key = struct
    type t = coordinate [@@deriving sexp, compare, equal]
    let to_string (k : t) = Sexp.to_string (sexp_of_t k)
end

module Coordinate_map = Map.Make(Coordinate_key)
module Coordinate_set = struct 
    include Set.Make(Coordinate_key)
    let to_string (set : t) = Sexp.to_string (sexp_of_t set)
end 

type t = {
    cells : Coordinate_set.t;
    width : int;
    height : int;
}
let empty = {
    cells = Coordinate_set.empty;
    width = -1;
    height = -1
}

let in_bounds ({ x; y } : coordinate) ~(width : int) ~(height : int) =
    x >= 0 && x < width && y >= 0 && y < height

let surrounding ({ x; y } : coordinate) : Coordinate_set.t = Coordinate_set.of_list [
    { x; y = (y + 1) }; { x = (x + 1); y = (y + 1) }; { x = (x - 1); y = (y + 1) }; (* bottom 3 *)
    { x = (x - 1); y }; { x = (x + 1); y }; (* left and right *)
    { x; y = (y - 1) }; { x = (x + 1); y = (y - 1) }; { x = (x - 1); y = (y - 1) } (* top 3 *)
]

let neighbors ({ x; y }: coordinate) ~(width : int) ~(height : int) : Coordinate_set.t =
    if not (in_bounds { x; y } ~width ~height) then Coordinate_set.empty
    else 
        surrounding { x; y }
        |> Set.filter ~f:(fun coord -> in_bounds coord ~width ~height)

let get_count (map : int Coordinate_map.t) (coordinate : coordinate) : int =
    match Map.find map coordinate with
    | None -> 0
    | Some count -> count;;

let get_count_map (map : int Coordinate_map.t) (candidates : Coordinate_set.t) ~(alive : Coordinate_set.t) : int Coordinate_map.t  =
    printf "Alive cells are %s\n" (Coordinate_set.to_string alive);
    Set.fold candidates ~init:map ~f:(fun acc candidate ->
        get_count acc candidate
        |> (fun count ->
            if Set.mem alive candidate then Map.set acc ~key:candidate ~data:(count + 1)
            else acc
        )
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
    Set.fold cells ~init:Coordinate_set.empty ~f:(fun acc coord ->
        let candidates = neighbors coord ~width ~height in
        Set.fold candidates ~init:(acc) ~f:(fun acc candidate ->
            let all_neighbors = neighbors candidate ~width ~height in
            let alive_neighbors = Set.inter all_neighbors cells in
            if (Set.length alive_neighbors) = 3 then Set.add acc candidate
            else acc
        ) 
    )
    |> (fun set -> printf "set is %s\n" (Coordinate_set.to_string set); set)

(** [next curr] is the new grid state given the curr state *)
let next ({ cells; width; height } : t) : t =
    let spawned = spawn_set { cells; width; height } in
    let survived = survive_set { cells; width; height } in
    { cells = Set.union spawned survived; width; height }
