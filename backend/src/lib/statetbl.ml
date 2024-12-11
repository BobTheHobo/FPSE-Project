open Core

type base_grid_map = Maker.T.t Coordinate.CoordinateMap.t

type t = {
  grid_module : (module Map_grid.S with type t = base_grid_map);
  obstacles : base_grid_map;
  player_position : Coordinate.t;
  is_dead : bool;
  height : int;
  width : int;
}

type encodeable_t = {
  obstacles : base_grid_map;
  player_position : Coordinate.t;
  is_dead : bool;
  height : int;
  width : int;
}

let to_string (t : t) =
  let (module A) = t.grid_module in
  Printf.sprintf
    ("{\n" ^^ "  player_position: { x: %d, y: %d },\n" ^^ "  is_dead: %s,\n"
   ^^ "  obstacles: %s\n  is_dead: %s\n" ^^ "}")
    t.player_position.x t.player_position.y (Bool.to_string t.is_dead)
    (A.to_string t.obstacles) (Bool.to_string t.is_dead)

let tbl : (string, t) Hashtbl.t = Hashtbl.create (module String)
let get (key : string) : t option = Hashtbl.find tbl key
let get_exn (key : string) : t = Hashtbl.find_exn tbl key
let delete (key : string) = Hashtbl.remove tbl key

let string_of_id (id : string) =
  match get id with
  | None -> failwith "Can't convert a state that doesn't exist to a string..."
  | Some v -> to_string v

let set ~(key : string) (state : t) : unit = Hashtbl.set tbl ~key ~data:state

let update ~(id : string) ~player_position ~is_dead ~obstacles =
  let curr_state = get_exn id in
  let new_state =
    {
      grid_module = curr_state.grid_module;
      player_position;
      is_dead;
      obstacles;
      height = curr_state.height;
      width = curr_state.width;
    }
  in
  set ~key:id new_state
