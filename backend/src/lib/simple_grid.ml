open Core

type coordinate = { x : int; y : int } [@@deriving sexp, compare, equal]

module Coordinate_key = struct
    type t = coordinate [@@deriving sexp, compare, equal]
end

module Coordinate_map = Map.Make(Coordinate_key)

type t = coordinate list Coordinate_map.t
let empty = Coordinate_map.empty

let in_bounds ({ x; y } : coordinate) ~(width : int) ~(height : int) =
    x >= 0 && x < width && y >= 0 && y < height

let surrounding ({ x; y } : coordinate) : coordinate list = [
    { x; y = (y + 1) }; { x = (x + 1); y = (y + 1) }; { x = (x - 1); y = (y + 1) }; (* bottom 3 *)
    { x = (x - 1); y }; { x = (x + 1); y }; (* left and right *)
    { x; y = (y - 1) }; { x = (x + 1); y = (y - 1) }; { x = (x - 1); y = (y - 1) } (* top 3 *)
]

let neighbors ({ x; y }: coordinate) ~(width : int) ~(height : int) : coordinate list =
    if not (in_bounds { x; y } ~width ~height) then []
    else match x, y with
    | xtl, ytl when xtl = 0 && ytl = 0 -> [
        { x = 1; y = 0 }; { x = 1; y = 1 }; { x = 0; y = 1 }
    ]
    | xtr, ytr when xtr = (width - 1) && ytr = 0 -> [
        { x = (width - 2); y = 0 }; { x = (width - 2); y = 1 }; { x = (width - 1); y = 1 }
    ]
    | xbl, ybl when xbl = 0 && ybl = (height - 1) -> [
        { x = 0; y = (height - 2) }; { x = 1; y = (height - 2) }; { x = 1; y = (height - 1) }
    ]
    | xbr, ybr when xbr = (width - 1) && ybr = (height - 1) -> [
        { x = (width - 1); y = (height - 2) }; { x = (width - 2); y = (height - 2) }; { x = (width - 2); y = (height - 1) }
    ]
    | x, y -> surrounding { x; y }

(** [next curr] is the new grid state given the curr state *)
let next (curr : t) =
    Map.to_alist curr 
    |> List.filter ~f:(fun (_, neighbors) ->
        List.length neighbors > 1 && List.length neighbors < 4
    )
