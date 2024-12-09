open Fetch
open React
open Belt.Array
open Js.Json


@val external addEventListener: (string, ReactEvent.Keyboard.t => unit) => unit = "document.addEventListener"
@val external removeEventListener: (string, ReactEvent.Keyboard.t => unit) => unit = "document.removeEventListener"

let getColorClass = (state: int) => {
  switch state {
  | 0 => "traversable"
  | 1 => "goal"
  | 2 => "player"
  | 3 => "fire"
  | 4 => "ice"
  | _ => "default"
  }
}

type position = {
  x : int,
  y : int
}

let encode_request_body = (player_position: position) => 
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

let tupleToPosition = ((x, y): (int, int)) : position => { x, y }


@react.component
let make = () => {
  let (position, setPosition) = useState(() => (0, 0))
  let (fire, setFire) = useState(() => [])
  let (ice, setIce) = useState(() => [])
  let (isValidMove, setIsValidMove) = useState(() => true)
  let gridSize = 15
  let maxX = gridSize - 1
  let maxY = gridSize - 1

  let grid = make(gridSize, ())->map(() => make(gridSize, 0))
  
  let fetchObstacles = () => {
    if (isValidMove) {
      let gameCall = async () => {
        let response = await fetch("http://localhost:8080/game", {
          method: #POST,
          body: encode_request_body(tupleToPosition(position)),
          headers: Headers.fromObject({
            "Content-Type": "application/json"
          }),
          credentials: #"include"
        });
        let payload = await Response.json(response);
        let decoded = decode_response_body(payload);
        let obstaclesRaw = Js.Dict.unsafeGet(decoded, "obstacles");
        let fireTuples = obstaclesToTuples(obstaclesRaw, "Fire");
        let iceTuples = obstaclesToTuples(obstaclesRaw, "Ice");
        Js.log("Fire tuples");
        Js.log(fireTuples);
        Js.log("Ice tuples");
        Js.log(iceTuples);
        if (Array.length(fireTuples) > 0) {
          setFire(_ => fireTuples);
        }
        if (Array.length(iceTuples) > 0) {
          setIce(_ => iceTuples);
        } 
        // setIce(_ => iceTuples);
      }
      // let fetchCall = async () => {
      //   let response = await fetch(
      //     "http://localhost:8080/get_obstacles",
      //     {
      //       method: #POST,
      //       body: { "fire": fire, "ice": ice, "player": position }->stringifyAny->Belt.Option.getExn->Body.string,
      //       headers: Headers.fromObject({
      //         "Content-type": "application/json",
      //       }),
      //     },
      //   )

      //   let json_out = await response->Response.json
      //   Js.log(json_out)
      //   let dict_out = switch decodeObject(json_out) {
      //   | Some(dict_data) => dict_data
      //   | None => let dict_data = Js.Dict.empty()
      //     Js.Dict.set(dict_data, "fire", Js.Json.Array([]))
      //     Js.Dict.set(dict_data, "ice", Js.Json.Array([]))
      //     Js.Dict.set(dict_data, "player", Js.Json.Array([
      //       Js.Json.number(0.0),
      //       Js.Json.number(0.0),
      //     ]))
      //     dict_data
      //   }
      //   setFire(_ => dict_out->Js.Dict.unsafeGet("fire")->decode_obstacles)
      //   setIce(_ => dict_out->Js.Dict.unsafeGet("ice")->decode_obstacles)
      // }

      gameCall()
      |> ignore
    }
  }

  let onKeyDown = evt => {
    let key = ReactEvent.Keyboard.key(evt)

    Js.log(position)
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

  React.useEffect(() => {
    fetchObstacles()
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
  fire->forEach(pos => {
        let (x, y) = pos
        grid->getExn(x)->setExn(y, 3)})
  ice->forEach(pos => {
        let (x, y) = pos
        grid->getExn(x)->setExn(y, 4)})

  let className = "grid"
  <div className={className ++ " border border-red-500"}>
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
