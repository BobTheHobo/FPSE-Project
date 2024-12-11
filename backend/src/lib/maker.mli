open Map_grid

module type S = sig
  type t = 
    | Fire
    | Water 
    | Ice [@@deriving sexp, compare]

  val to_string : t -> string
  val on_collision : t -> t -> t option
end
module T : S

type game_params = { fire : Params.t; ice : Params.t; water : Params.t }

val make_params_of_t : game_params -> (T.t -> Params.t)

val make_grid :
  (T.t -> Params.t) -> (module Map_grid.S with type t = T.t Coordinate.CoordinateMap.t)
