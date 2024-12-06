open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives


type coordinate = { x : int; y : int }
[@@deriving yojson, sexp, compare]

module CoordinateComparator = struct
  type t = coordinate [@@deriving compare, sexp]

  module Cmp = Comparator.Make (struct
    type t = coordinate [@@deriving compare, sexp]
  end)

  type comparator_witness = Cmp.comparator_witness

  let comparator : (t, comparator_witness) Comparator.t = Cmp.comparator
end

module CoordinateSet = struct
  include Set.Make (CoordinateComparator)

  let t_of_yojson json =
    match json with
    | `List lst ->
        List.map lst ~f:coordinate_of_yojson |> of_list
    | _ -> failwith "Invalid JSON format for CoordinateSet"

  let yojson_of_t t =
    `List (Set.to_list t |> List.map ~f:yojson_of_coordinate)
end


type obstacle_state = {
  fire : CoordinateSet.t;
  ice : CoordinateSet.t;
}
[@@deriving yojson]

type game_status =
  | Ongoing
  | Burnt
  | Froze
  | Won
[@@deriving yojson]

type game_state = {
  player : coordinate;
  goal : coordinate;
  start_obstacles : obstacle_state;
  current_obstacles : obstacle_state;
  state : game_status;
}
[@@deriving yojson]
