(*
  FPSE Project
  
  Simple CLI for testing and development
*)

open Core

let () = 
  match Sys.get_argv () |> Array.to_list with
  | _ :: grid_rows :: grid_cols :: other_args -> begin
    print_string "Grid rows: "; print_string @@ grid_rows ^ "\n"; 
    print_string "Grid cols: "; print_string @@ grid_cols ^ "\n";
    Game.Input.start_game;
    match other_args with
    | [] ->  ()
    | args -> print_string "Other arguments: "; List.iter args ~f:(fun arg -> print_string arg);
  end
  | _ -> eprintf "Invalid args: Please provide grid rows and grid columns\n"
