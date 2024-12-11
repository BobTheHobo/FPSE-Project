open Core
open Map_grid

type game_params = { fire : Params.t; ice : Params.t; water : Params.t }

module T = struct
  type t = Fire | Ice | Water [@@deriving sexp, compare]

  let to_string (t : t) = Sexp.to_string (sexp_of_t t)
  let cell_ls = [ Fire; Ice; Water ]

  let on_collision (a : t) (b : t) =
    if compare a b = 0 then Some a
    else
      match (a, b) with
      | Fire, Ice -> Some Water
      | Ice, Water -> Some Ice
      | _ -> None
end

let make_params_of_t ({ fire; ice; water } : game_params) =
 fun (t : T.t) -> match t with T.Fire -> fire | Ice -> ice | Water -> water

let make_grid params_of_t =
  let module S = Map_grid.Make (struct
    include T
    module CellSet = Set.Make (T)

    let all_set = CellSet.of_list [ Fire; Ice; Water ]
    let params_of_t = params_of_t
  end) in
  (module S : Map_grid.S with type t = T.t Coordinate.CoordinateMap.t)
