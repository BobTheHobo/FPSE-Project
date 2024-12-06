(** Core functionality for the game grid *)

val in_bounds : (int * int) -> int -> int -> bool
(** [in_bounds coord width height] checks if [coord] is within the grid bounds defined by [width] and [height]. *)

val neighbors : (int * int) -> int -> int -> Types.CoordinateSet.t
(** [neighbors coord width height] returns the set of all valid neighbors of [coord]
    within the grid of size [width] x [height]. *)

val random_obstacles :
  int -> int -> int -> Types.CoordinateSet.t -> Types.CoordinateSet.t
(** [random_obstacles count width height exclusions] generates a set of [count] random
    obstacle positions within the grid, ensuring no overlap with [exclusions]. *)
