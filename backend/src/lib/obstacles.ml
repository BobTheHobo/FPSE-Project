open Core
open Types
open Game_grid

(** The ConwayRules functor for generic obstacle behavior. *)
module ConwayRules (Grid : sig
  val neighbors : (int * int) -> int -> int -> CoordinateSet.t
end) (Logic : sig
  val should_survive : int -> bool
  val should_spawn : int -> bool
end) : Obstacle = struct
  type t = {
    cells : CoordinateSet.t;
    width : int;
    height : int;
  }

  let init coords width height = { cells = coords; width; height }

  let next obstacle =
    let { cells; width; height } = obstacle in
    let survive_set =
      CoordinateSet.filter cells ~f:(fun coord ->
          let neighbor_count = CoordinateSet.cardinal (Grid.neighbors coord width height |> CoordinateSet.inter cells) in
          Logic.should_survive neighbor_count)
    in
    let spawn_set =
      CoordinateSet.fold cells ~init:CoordinateSet.empty ~f:(fun acc coord ->
          let candidates = Grid.neighbors coord width height in
          CoordinateSet.fold candidates ~init:acc ~f:(fun acc candidate ->
              let neighbor_count = CoordinateSet.cardinal (Grid.neighbors candidate width height |> CoordinateSet.inter cells) in
              if Logic.should_spawn neighbor_count then CoordinateSet.add acc candidate else acc))
    in
    { cells = CoordinateSet.union survive_set spawn_set; width; height }

  let coordinates obstacle = obstacle.cells
end

(** Module implementing fire obstacle behavior. *)
module Fire = struct
  module Logic = struct
    let should_survive n = n = 2 || n = 3
    let should_spawn n = n = 3
  end

  include ConwayRules (Game_grid) (Logic)
end

(** Module implementing ice obstacle behavior. *)
module Ice = struct
  module Logic = struct
    let should_survive n = n > 3
    let should_spawn n = n = 2
  end

  include ConwayRules (Game_grid) (Logic)
end

let resolve_overlaps fire ice goal =
  let overlap = CoordinateSet.inter fire ice in
  let fire_clean = CoordinateSet.diff fire overlap in
  let ice_clean = CoordinateSet.diff ice overlap in
  let fire_clean = CoordinateSet.remove fire_clean goal in
  let ice_clean = CoordinateSet.remove ice_clean goal in
  (fire_clean, ice_clean)