(** Utility functions for the game *)

val generate_random_coordinates :
  int -> int -> int -> Types.CoordinateSet.t -> Types.CoordinateSet.t
(** [generate_random_coordinates n width height exclusions] generates [n] unique random
    coordinates within the grid of size [width] x [height], ensuring no overlap with [exclusions]. *)

val set_intersection :
  Types.CoordinateSet.t -> Types.CoordinateSet.t -> Types.CoordinateSet.t
(** [set_intersection set1 set2] returns the intersection of two sets [set1] and [set2]. *)

val set_difference :
  Types.CoordinateSet.t -> Types.CoordinateSet.t -> Types.CoordinateSet.t
(** [set_difference set1 set2] returns the elements in [set1] that are not in [set2]. *)

val is_within_bounds : (int * int) -> int -> int -> bool
(** [is_within_bounds coord width height] checks if [coord] is within the grid bounds defined by [width] and [height]. *)

val parse_body : string -> (Types.game_state, string) Result.t
(** [parse_body body] parses a JSON string [body] into a game state. *)

val parse_move : string -> ((int * int), string) Result.t
(** [parse_move body] parses a JSON string [body] into a player move. *)
