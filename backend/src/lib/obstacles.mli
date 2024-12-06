(** Modules and functionality for obstacles in the game *)

(** The signature for a generic obstacle module. *)
module type Obstacle = sig
    type t
    (** The type representing the state of the obstacle. *)
  
    val init : Types.CoordinateSet.t -> int -> int -> t
    (** [init coords width height] initializes the obstacle with the given [coords] and grid dimensions. *)
  
    val next : t -> t
    (** [next obstacle] computes the next state of the obstacle based on its specific rules. *)
  
    val coordinates : t -> Types.CoordinateSet.t
    (** [coordinates obstacle] returns the current set of coordinates occupied by the obstacle. *)
  end
  
(** The ConwayRules functor for generic obstacle behavior. *)
module ConwayRules : functor
  (Grid : sig
      val neighbors : (int * int) -> int -> int -> Types.CoordinateSet.t
    end) ->
  functor
    (Logic : sig
        val should_survive : int -> bool
        (** Determines if a cell survives based on the number of neighbors. *)

        val should_spawn : int -> bool
        (** Determines if a new cell is spawned based on the number of neighbors. *)
      end) ->
    Obstacle

(** Module implementing fire obstacle behavior. *)
module Fire : Obstacle

(** Module implementing ice obstacle behavior. *)
module Ice : Obstacle

val resolve_overlaps :
  Types.CoordinateSet.t ->
  Types.CoordinateSet.t ->
  (int * int) ->
  Types.CoordinateSet.t * Types.CoordinateSet.t

(** [resolve_overlaps fire ice goal] removes overlaps between the [fire] and [ice] obstacles,
    and ensures neither overlaps with the [goal]. *)
  