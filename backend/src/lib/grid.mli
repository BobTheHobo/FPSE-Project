open Core

module type Cell = sig
    (* Cell type *)
    type t

    val default : t
    val to_string : t -> string
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
end

type 'a grid = {
    cells : 'a list list;
    width : int;
    height : int;
}

module type Grid = functor (Cell : Cell) -> sig
    (* Grid type is a 2D list of cells *)
    type t = Cell.t grid

    val create_grid : int -> int -> t
    val set_cell : t -> int -> int -> Cell.t -> t
    val get_index : t -> int -> int -> int
    val get_cell : t -> int -> int -> Cell.t
    val draw_grid : t -> unit
end

module Make : Grid

module StrCell : Cell
