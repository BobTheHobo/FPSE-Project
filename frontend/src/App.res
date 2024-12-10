open React
open Fetch
open Js.Json

let defaultOptions: GameConfigForm.game_params = {
  b: 3,
  s1: 2,
  s2: 3,
}

type coordinate = { x : int, y : int }
type obstacle = {
  coordinate: coordinate,
  cell_types: array<string>
}
type game_state = {
  obstacles: array<obstacle>,
  is_dead: bool,
  player_position: coordinate
}

let defaultPosition = { x: -1, y: -1 }

let defaultGameState = {
  obstacles: [],
  is_dead: true,
  player_position: defaultPosition
}

let decodeArrayFromJson = (array : Js.Json.t) => {
  switch (decodeArray(array)) {
    | Some(v) => v
    | None => []
  }
}

let decodeIntFromJson = (json : Js.Json.t) => {
  switch (decodeNumber(json)) {
    | Some(v) => Js.Math.floor_int(v)
    | None => -1
  }
}

let decodeCoordinateFromJson = (coordinate : Js.Json.t) => {
  switch (decodeObject(coordinate)) {
    | Some(v) => {
      let xJson = Dict.getUnsafe(v, "x")
      let yJson = Dict.getUnsafe(v, "y")
      {
        x: decodeIntFromJson(xJson),
        y: decodeIntFromJson(yJson)
      }
    }
    | None => defaultPosition
  }
}

let decodeObstacleObject = (obstacleJson: Js.Json.t) => {
  switch (decodeObject(obstacleJson)) {
    | Some (dict) => {
      let coordinate = Dict.getUnsafe(dict, "coordinate") |> decodeCoordinateFromJson
      let cell_types = Dict.getUnsafe(dict, "cell_types") 
      |> decodeArrayFromJson
      |> (array) => Js.Array.map(cellType => Js.Json.decodeString(cellType) |> stringOption => Option.getExn(stringOption), array);
      ({
        coordinate,
        cell_types
      })
    }
    | None => ({
        coordinate: defaultPosition,
        cell_types: []
      })
  }
}

let decodeBooleanFromJson = (boolean: Js.Json.t) => {
  switch (decodeBoolean(boolean)) {
    | Some(v) => v
    | None => false
  }
}

let decodeDict = (dict : dict<Js.Json.t>) => {
  let is_dead = Js.Dict.unsafeGet(dict, "is_dead") |> decodeBooleanFromJson
  let player_position = Js.Dict.unsafeGet(dict, "player_position") |> decodeCoordinateFromJson
  let obstacles = Js.Dict.unsafeGet(dict, "obstacles") |> decodeArrayFromJson |> array => Js.Array.map(json => decodeObstacleObject(json), array)
  {
    is_dead,
    player_position,
    obstacles,
  }
}

let decodeResponseBody = (payload) => {
  switch decodeObject(payload) {
    | Some(dict) => decodeDict(dict)
    | None => defaultGameState
  }
}

@react.component
let make = () => {
  let (fireParams, setFireParams) = useState(_ => defaultOptions)
  let (iceParams, setIceParams) = useState(_ => defaultOptions)
  let (waterParams, setWaterParams) = useState(_ => defaultOptions)
  let (initialGameState, setInitialGameState) = useState(_ => defaultGameState)

  let should_show_game = initialGameState.obstacles->Array.length > 0

  let grid = if should_show_game {
    <Grid />
  } else {
    <Placeholder />
  }

  let startGame = () => {
    let fetchCall = async () => {
      let response = await fetch("http://localhost:8080/game/new", {
        method: #POST,
        body: {
          "fire": fireParams,
          "ice": iceParams,
          "water": waterParams,
          "width": 20,
          "height": 20
        }->stringifyAny->Belt.Option.getExn->Body.string,
        headers: Headers.fromObject({
          "Content-Type": "application/json"
        }),
        credentials: #"include"
      })
      let payload = await Response.json(response)
      let decoded = decodeResponseBody(payload)
      setInitialGameState(_ => decoded)
      Js.log(decoded)
    }
    fetchCall()
    |> ignore
  }

  <main className="bg-gray-900 grid grid-cols-7 min-h-svh">
    <div className="col-span-2 border-r border-gray-400 p-2">
      <div className="flex flex-col gap-1 border-b border-gray-400 px-2">
        <h1 className="text-white text-xl"> {"Can you beat the Game of Life?"->React.string} </h1>
        <h3 className="text-white"> {"You: 0"->React.string} </h3>
        <h3 className="text-white"> {"Computer: 0"->React.string} </h3>
      </div>
      <div className="p-4">
        <GameConfigForm value={fireParams} onValueChange={setFireParams} cellType="Fire"/>
        <GameConfigForm value={iceParams} onValueChange={setIceParams} cellType="Ice"/>
        <GameConfigForm value={waterParams} onValueChange={setWaterParams} cellType="Water"/>
        <button className="mt-4 border border-gray-600 rounded-md h-12 px-4 text-white bg-blue-700" onClick={_ => startGame()}>
          {"Begin game"->React.string}
        </button>
      </div>
    </div>
    <div className="col-span-5 flex justify-center mt-16"> {grid} </div>
  </main>
}
