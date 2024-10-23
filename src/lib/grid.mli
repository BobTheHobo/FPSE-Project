module type Cell = sig
    (* Cell type *)
    type ct

    val default : ct
    val to_string : ct -> string
end

module type Grid = sig
    include Cell

    (* Grid type is a 2D list of cells *)
    type t

    val create_grid : int -> int -> t
    val set_cell : t -> int -> int -> ct -> t
    val get_index : t -> int -> int -> int
    val get_cell : t -> int -> int -> ct
    val draw_grid : t -> unit
end

type 'a grid = {
    cells : 'a list;
    width : int;
    height : int;
}

module type G = functor (Cell : Cell) -> Grid with type t = Cell.ct grid

module Make : G

module StrCell : Cell
