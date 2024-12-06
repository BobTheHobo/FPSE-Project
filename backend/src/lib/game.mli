(** Core game logic and state transitions *)

val init_state : (int * int) -> int -> int -> Types.game_state
(** [init_state start width height] initializes the game with the player's [start] position,
    grid dimensions [width] and [height], and randomly generated obstacles. *)

val next_state : (int * int) -> Types.game_state -> Types.game_state
(** [next_state move state] computes the next game state based on the player's [move]
    and the current [state]. *)
    