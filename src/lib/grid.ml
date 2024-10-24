open Core

(* Silence unused variable warning *)
[@@@ocaml.warning "-27"]

module type Cell = sig
    (* Cell type *)
    type ct

    val default : ct
    val to_string : ct -> string
end

module type Grid = sig
    include Cell

    type t = {
        cells : ct list;
        width : int;
        height : int;
    }

    val create_grid : int -> int -> t
    val set_cell : t -> int -> int -> ct -> t
    val get_index : t -> int -> int -> int
    val get_cell : t -> int -> int -> ct
    val draw_grid : t -> unit
end

module type G = functor (Cell : Cell) -> Grid with type ct = Cell.ct

(* Define functor used to create grids *)
module Make : G = functor (Cell : Cell) ->
struct
    include Cell

    (* Cells, width, height *)
    type t = {
        cells : ct list;
        width : int;
        height : int;
    }

    (* Create empty grid *)
    let create_grid (w : int) (h : int) : t =
        let cells = List.init (h * w) ~f:(fun _ -> Cell.default) in
        {cells; width = w; height = h}
        
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
    let get_cell (grid : t) (row : int) (col : int) : ct =
        (* should eventually check if is out of bounds and do something *)
        List.nth_exn grid.cells (get_index grid row col)

    let is_out_of_bounds (grid : t) (row : int) (col : int) : bool = 
        if row < 0 || row >= grid.height || col < 0 || col >= grid.width then (
            print_string "Cell position out of bounds"; true
        ) else false

    let set_cell (grid : t) (row : int) (col : int) (value : ct) : t = 
        if is_out_of_bounds grid row col then grid 
        else
            let idx = get_index grid row col in
            let new_cells = 
                List.mapi grid.cells ~f:(fun i cell ->
                    if i = idx then value else cell
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
  type ct = string
  let default = " "
  let to_string x = x
end
