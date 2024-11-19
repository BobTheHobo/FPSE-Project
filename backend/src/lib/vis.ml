open Core

(* Silence unused variable warning *)
[@@@ocaml.warning "-27"]

module Base_grid = Grid.Make(Base_cell)

(* Eventually use CellVis to specify what to use for the different ice/fire cells *)
module type CellVis = sig
  val cell_default : string (* String used for empty cells *)
  val cell_alive : string (* String used for alive cells *)
  val player : string (* String used for player *)
end

module ConwayCellVis : CellVis = struct
  let cell_default = " "
  let cell_alive = "*"
  let player = "!"
end

(* Functor that uses a CellVis that specifies the strings to use for default and alive cells *)
module MakeGridVis =
functor
  (CellVis : CellVis)
  ->
  struct
    (* Visualized grid of of type string list list  *)
    type t = { cells : string list list; width : int; height : int }

    let default_str = CellVis.cell_default
    let alive_str = CellVis.cell_alive
    (* let player_str = CellVis.player *)

    (* Create grid from simple grid *)
    let create_grid_vis (grid : Base_grid.t) : t =
      let ls =
        List.init grid.height ~f:(fun yi ->
            List.init grid.width ~f:(fun xi ->
                let coord : Base_cell.t = { x = xi; y = yi } in
                if Set.mem grid.cells coord then alive_str else default_str))
      in
      { cells = ls; width = grid.width; height = grid.height }

    let draw_col_labels (width : int) : unit =
      (* Front spacing for row labels *)
      let front_spacing = "  " in

      let num_str : string =
        width
        |> List.init ~f:(fun i -> Int.to_string @@ (i mod 10))
        |> String.concat (* Combine numbers into one string *)
        |> ( ^ ) front_spacing (* Add front spacing *)
      in

      let divider_str : string =
        width
        |> List.init ~f:(fun _ -> "_")
        |> String.concat (* Combine spacing chars *)
        |> ( ^ ) front_spacing (* Add front spacing *)
      in

      print_string @@ num_str ^ "\n" ^ divider_str ^ "\n"

    (* Get the content of a specific cell *)
    let get_cell (grid_vis : t) (row : int) (col : int) : string =
      (* should eventually check if is out of bounds and do something *)
      let row_hd = List.nth_exn grid_vis.cells row in
      List.nth_exn row_hd col

    let is_out_of_bounds (grid_vis : t) (row : int) (col : int) : bool =
      (* Precondition check to make sure nested lists are the same length *)
      List.iter grid_vis.cells ~f:(fun nested ->
          assert (grid_vis.width = List.length nested));

      if row < 0 || row >= grid_vis.height || col < 0 || col >= grid_vis.width
      then (
        print_string "Cell position out of bounds";
        true)
      else false

    (* Setting cells should be handled in Simple_grid? *)
    (* let set_cell (grid : t) (row : int) (col : int) (value : Cell.t) : t =  *)
    (*     if is_out_of_bounds grid row col then grid  *)
    (*     else *)
    (*         let new_cells =  *)
    (*             List.mapi grid.cells ~f:(fun i row_hd -> *)
    (*                 if i = row then  *)
    (*                     List.mapi row_hd ~f:(fun j cell -> *)
    (*                         if j = col then value else cell *)
    (*                     ) *)
    (*                 else row_hd *)
    (*             ) *)
    (*         in *)
    (*         {grid with cells = new_cells} *)

    (* Takes in a Simple_grid, creates a grid_vis, and then prints it *)
    let draw_grid (grid : Base_grid.t) : unit =
      let grid_vis = create_grid_vis grid in

      draw_col_labels grid.width;

      List.iteri grid_vis.cells ~f:(fun row_i row ->
          print_string @@ Int.to_string (row_i mod 10) ^ "|";
          (* Print row label *)
          List.iter row ~f:(fun cell -> print_string cell);
          print_string "\n")
  end
