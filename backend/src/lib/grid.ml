open Core

(* Silence unused variable warning *)
[@@@ocaml.warning "-27"]

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

(* Define functor used to create grids *)
module Make = functor (Cell : Cell) ->
struct
    (* Cells, width, height *)
    type t = Cell.t grid

    (* Create empty grid *)
    let create_grid (w : int) (h : int) : t =
        let cells = List.init (h) ~f:(fun _ -> List.init w ~f:(fun _ -> Cell.default)) in
        { cells; width = w; height = h }
        
    let draw_col_labels (width : int) : unit =
        (* Front spacing for row labels *)
        let front_spacing = "  " in

        let num_str : string = 
            width
            |> List.init ~f:(fun i -> Int.to_string @@ (i mod 10))
            |> String.concat (* Combine numbers into one string *)
            |> (^) front_spacing (* Add front spacing *)
        in

        let divider_str : string = 
            width
            |> List.init ~f:(fun _ -> "_")
            |> String.concat (* Combine spacing chars *)
            |> (^) front_spacing (* Add front spacing *)
        in

        print_string @@ num_str ^ "\n" ^ divider_str ^ "\n"

    let get_index (grid : t) (row : int) (col : int) : int =
        grid.width * row + col

    (* Get the content of a specific cell *)
    let get_cell (grid : t) (row : int) (col : int) : Cell.t =
        (* should eventually check if is out of bounds and do something *)
        let row_hd = List.nth_exn grid.cells row in
        List.nth_exn row_hd col

    let is_out_of_bounds (grid : t) (row : int) (col : int) : bool = 
        if row < 0 || row >= grid.height || col < 0 || col >= grid.width then (
            print_string "Cell position out of bounds"; true
        ) else false

    let set_cell (grid : t) (row : int) (col : int) (value : Cell.t) : t = 
        if is_out_of_bounds grid row col then grid 
        else
            let new_cells = 
                List.mapi grid.cells ~f:(fun i row_hd ->
                    if i = row then 
                        List.mapi row_hd ~f:(fun j cell ->
                            if j = col then value else cell
                        )
                    else row_hd
                )
            in
            {grid with cells = new_cells}

    let draw_grid (grid : t) : unit =

        draw_col_labels grid.width;

        let rec aux row_i =

            let h = grid.height in
            if row_i < h then begin
                print_string @@ (Int.to_string (row_i mod 10)) ^ "|";
                let row = List.init grid.width ~f:(fun col_i -> get_cell grid row_i col_i) in

                List.iter row ~f:(fun cell ->
                    print_string @@ Cell.to_string cell;
                );
                print_string "\n";
                aux (row_i + 1)
            end
        in
        aux 0
end

(* String Cell *)
module StrCell : Cell = struct
    type t = string [@@deriving sexp, compare, equal]
    let default = ""
    let to_string = String.to_string
    let sexp_of_t = String.sexp_of_t
    let t_of_sexp = String.t_of_sexp
    let equal = String.equal
    let compare = String.compare
end
