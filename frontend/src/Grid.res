open Fetch
open React
open Belt.Array
open Js.Json

type coordinate = { x : int, y : int }
type obstacle = {
  coordinate: coordinate,
  cell_type: string
}
type game_state = {
  obstacles: array<obstacle>,
  is_dead: bool,
  player_position: coordinate
}


@val external addEventListener: (string, ReactEvent.Keyboard.t => unit) => unit = "document.addEventListener"
@val external removeEventListener: (string, ReactEvent.Keyboard.t => unit) => unit = "document.removeEventListener"

let getColorClass = (state: int) => {
  switch state {
  | 0 => "traversable"
  | 1 => "goal"
  | 2 => "player"
  | 3 => "fire"
  | 4 => "ice"
  | 5 => "water"
  | _ => "default"
  }
}

let encode_request_body = (player_position: coordinate) => 
{
  "player_position": player_position
}->stringifyAny->Belt.Option.getExn->Body.string

let decode_response_body = (payload) => switch decodeObject(payload) {
  | Some(response_body) => response_body
  | None => Js.Dict.empty()
}

let decodeCoordinate = coordinate => switch decodeObject(coordinate) {
  | Some(obj) => switch (Js.Dict.get(obj, "x"), Js.Dict.get(obj, "y")) {
    | (Some(Js.Json.Number(x)), Some(Js.Json.Number(y))) => { x: Js.Math.floor(x), y: Js.Math.floor(y) }
    | _ => { x: -1, y: -1 }
  }
  | _ => { x: -1, y : -1 }
}

let decodeCellTypes = cellTypes => switch decodeArray(cellTypes) {
  | Some(array) => array->Belt.Array.keepMap(Js.Json.decodeString)
  | None => []
}

let decodeObstacle = (obstacle, cellType) => switch decodeObject(obstacle) {
  | Some(obj) => {
    let coordinate = Js.Dict.unsafeGet(obj, "coordinate");
    let { x, y } = decodeCoordinate(coordinate);
    let cellTypes = Js.Dict.unsafeGet(obj, "cell_types");
    let foundIndex = decodeCellTypes(cellTypes)->Array.findIndex(cell => cell === cellType)
    if (foundIndex > -1) {
      (x, y)
    } else {
      (-1, -1)
    }
  }
  | None => (-1, -1)
}

let obstaclesToTuples = (obstacles, cellType) => switch decodeArray(obstacles) {
  | Some(array) => array->Belt.Array.map(a => decodeObstacle(a, cellType))->Array.filter(((x, y)) => x !== -1 && y !== -1)
  | None => []
}

let coordinateToTuple = ({ x, y }: coordinate) => (x, y)

let obstacleTuplesOfType = (obstacles: array<obstacle>, cellType: string) => 
  Js.Array.filter(obstacle => obstacle.cell_type == cellType, obstacles)
  |> filtered => Js.Array.map(obstacle => coordinateToTuple(obstacle.coordinate), filtered)

let tupleToCoordinate = ((x, y): (int, int)) : coordinate => { x, y }


@react.component
let make = (~gameState: game_state, ~onPlayerMove, ~isInteractive: bool) => {
  let firePositions = obstacleTuplesOfType(gameState.obstacles, "Fire")
  let icePositions = obstacleTuplesOfType(gameState.obstacles, "Ice")
  let waterPositions = obstacleTuplesOfType(gameState.obstacles, "Water")
  let playerPosition = coordinateToTuple(gameState.player_position)
  let (position, setPosition) = useState(() => playerPosition)
  let (isValidMove, setIsValidMove) = useState(() => true)
  let gridSize = 15
  let maxX = gridSize - 1
  let maxY = gridSize - 1
  let grid = make(gridSize, ())->map(() => make(gridSize, 0))

  let onKeyDown = evt => {
    if isInteractive {
      let key = ReactEvent.Keyboard.key(evt)

      switch key {
      | "ArrowLeft" =>
        // Js.log("Move left")
        setPosition(((x, y)) => {
          if (y - 1 >= 0) {
            setIsValidMove(_ => true)
            (x, Js.Math.max_int(y - 1, 0))
          } else {
            setIsValidMove(_ => false)
            (x, y)
          }
        })
        ReactEvent.Keyboard.preventDefault(evt)
      | "ArrowRight" =>
        // Js.log("Move right")
        setPosition(((x, y)) => {
          if (y + 1 <= maxY) {
            setIsValidMove(_ => true)
            (x, Js.Math.min_int(y + 1, maxY))
          } else {
            setIsValidMove(_ => false)
            (x, y)
          }
        })
        ReactEvent.Keyboard.preventDefault(evt)
      | "ArrowUp" =>
        // Js.log("Move up")
        setPosition(((x, y)) => {
          if (x - 1 >= 0) {
            setIsValidMove(_ => true)
            (Js.Math.max_int(x - 1, 0), y)
          } else {
            setIsValidMove(_ => false)
            (x, y)
          }
        })
        ReactEvent.Keyboard.preventDefault(evt)
      | "ArrowDown" =>
        // Js.log("Move Down")
        setPosition(((x, y)) => {
          if (x + 1 <= maxX) {
            setIsValidMove(_ => true)
            (Js.Math.min_int(x + 1, maxX), y)
          } else {
            setIsValidMove(_ => false)
            (x, y)
          }
        })
        ReactEvent.Keyboard.preventDefault(evt)
      | _ => ()
      }
    }
  }


  React.useEffect(() => {
    if isInteractive {
      addEventListener("keyup", onKeyDown)
    }
    Some(() => {
      removeEventListener("keyup", onKeyDown)
    })
  }, [isInteractive])

  React.useEffect(() => {
    if (isValidMove) {
      let encoded = tupleToCoordinate(position)
      onPlayerMove(encoded)
    }
    None
  }, [position])

  React.useEffect(() => {
    addEventListener("keyup", onKeyDown)
    Some(() => {
      removeEventListener("keyup", onKeyDown)
    })
  }, [])

  
  let (x, y) = position
  grid->getExn(maxX)->setExn(maxY, 1)
  grid->getExn(x)->setExn(y, 2)
  firePositions->forEach(pos => {
        let (x, y) = pos
        grid->getExn(x)->setExn(y, 3)})
  icePositions->forEach(pos => {
        let (x, y) = pos
        grid->getExn(x)->setExn(y, 4)})
  waterPositions->forEach(pos => {
    let (x, y) = pos
    grid->getExn(x)->setExn(y, 5)
  })

  <div className="h-[360px]">
    {grid
    ->Array.mapWithIndex((row, y) =>
      <div key={y->string_of_int} className="row">
        {row
        ->Array.mapWithIndex((state, x) =>
          <div key={x->string_of_int} className={"cell " ++ getColorClass(state)} />
        )
        ->React.array}
      </div>
    )
    ->React.array}
  </div>
}
