(** Shared types for the game *)

(** A type representing coordinates on the grid *)
type coordinate = { x : int; y : int }
[@@deriving yojson, sexp, compare]
(** [coordinate] defines a grid point with fields [x] and [y]. *)

(** A comparator module for the [coordinate] type *)
module CoordinateComparator : sig
  type t = coordinate
  (** Type of elements in the comparator, representing a coordinate. *)

  type comparator_witness
  (** A unique type for the comparator witness. *)

  val comparator : (t, comparator_witness) Core.Comparator.t
  (** The comparator instance for the [coordinate] type. *)
end

module CoordinateSet : sig
  include Core.Set.S with type elt = coordinate
  (** A set module for managing grid coordinates, implemented using [Set.S]. *)

  val t_of_yojson : Yojson.Safe.t -> t
  (** Converts a Yojson.Safe.t to a CoordinateSet.t. *)

  val yojson_of_t : t -> Yojson.Safe.t
  (** Converts a CoordinateSet.t to a Yojson.Safe.t. *)
end

(** The type representing the state of obstacles on the grid *)
type obstacle_state = {
  fire : CoordinateSet.t;  (** Set of coordinates for fire obstacles *)
  ice : CoordinateSet.t;   (** Set of coordinates for ice obstacles *)
}
[@@deriving yojson]
(** [obstacle_state] represents obstacles on the grid, categorized as fire or ice. *)

(** The possible states of the game *)
type game_status =
  | Ongoing  (** The game is still ongoing *)
  | Burnt    (** The player landed on a fire obstacle *)
  | Froze    (** The player landed on an ice obstacle *)
  | Won      (** The player reached the goal *)
[@@deriving yojson]

(** The type representing the full state of the game *)
type game_state = {
  player : coordinate;  (** The player's current position on the grid *)
  goal : coordinate;    (** The goal's position on the grid *)
  start_obstacles : obstacle_state;
  (** The initial obstacle positions for the game, used for regeneration *)
  current_obstacles : obstacle_state;
  (** The current obstacle positions after updates *)
  state : game_status;
  (** The current state of the game, representing its progress *)
}
[@@deriving yojson]
