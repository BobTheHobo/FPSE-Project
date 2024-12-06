open Core
open Types
open Utils
open Obstacles


let init_state start width height =
  let goal = (width - 1, height - 1) in
  let exclusions = CoordinateSet.of_list [start; goal] in
  let fire_coords = random_obstacles 5 width height exclusions in
  let exclusions = CoordinateSet.union exclusions fire_coords in
  let ice_coords = random_obstacles 5 width height exclusions in
  {
    player = start;
    goal;
    start_obstacles = { fire = fire_coords; ice = ice_coords };
    current_obstacles = { fire = fire_coords; ice = ice_coords };
    state = Ongoing;
  }


let check_end_conditions player state =
  let { current_obstacles; goal; _ } = state in
  if CoordinateSet.mem current_obstacles.fire player then
    { state with state = Burnt }
  else if CoordinateSet.mem current_obstacles.ice player then
    { state with state = Froze }
  else if player = goal then
    { state with state = Won }
  else state


let next_state move state =
  if state.state <> Ongoing then state
  else
    let player = (fst state.player + fst move, snd state.player + snd move) in
    if not (is_within_bounds player (fst state.goal + 1) (snd state.goal + 1)) then state
    else
      let state = { state with player } |> check_end_conditions player in
      if state.state <> Ongoing then state
      else
        let fire_next = Fire.next (Fire.init state.current_obstacles.fire (fst state.goal + 1) (snd state.goal + 1)) in
        let ice_next = Ice.next (Ice.init state.current_obstacles.ice (fst state.goal + 1) (snd state.goal + 1)) in
        let fire_coords, ice_coords = resolve_overlaps (Fire.coordinates fire_next) (Ice.coordinates ice_next) state.goal in
        let fire_unchanged = CoordinateSet.equal fire_coords state.current_obstacles.fire in
        let ice_unchanged = CoordinateSet.equal ice_coords state.current_obstacles.ice in
        let fire_final = if fire_unchanged then CoordinateSet.union fire_coords state.start_obstacles.fire else fire_coords in
        let ice_final = if ice_unchanged then CoordinateSet.union ice_coords state.start_obstacles.ice else ice_coords in
        let new_state =
          {
            state with
            current_obstacles = { fire = fire_final; ice = ice_final };
          }
          |> check_end_conditions player
        in
        new_state
