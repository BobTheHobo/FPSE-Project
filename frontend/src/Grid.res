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

let decodePosition = array => switch decodeArray(array) {
  | Some([a, b]) =>
    switch (decodeNumber(a), decodeNumber(b)) {
      | (Some(a), Some(b)) => (int_of_float(a), int_of_float(b))
      | _ => (0, 0)
    }
  | _ => (0, 0)
}

let decode_obstacles = obstacle_array => switch decodeArray(obstacle_array) {
| Some(arrayData) =>
    arrayData
    -> map(decodePosition)
    -> keepMap(pair => Some(pair))
| None =>
    []
}


@react.component
let make = () => {
  let (position, setPosition) = useState(() => (0, 0))
  let (fire, setFire) = useState(() => [])
  let (ice, setIce) = useState(() => [])
  let (isValidMove, setIsValidMove) = useState(() => true)
  let gridSize = 10
  let maxX = gridSize - 1
  let maxY = gridSize - 1

  let grid = make(gridSize, ())->map(() => make(gridSize, 0))
  
  let fetchObstacles = () => {
    if (isValidMove) {
      Js.log("HEREEEEE");
      let fetchCall = async () => {
        let response = await fetch(
          "http://localhost:8080/get_obstacles",
          {
            method: #POST,
            body: { "fire": fire, "ice": ice, "player": position }->stringifyAny->Belt.Option.getExn->Body.string,
            headers: Headers.fromObject({
              "Content-type": "application/json",
            }),
          },
        )

        let json_out = await response->Response.json
        Js.log(json_out)
        let dict_out = switch decodeObject(json_out) {
        | Some(dict_data) => dict_data
        | None => let dict_data = Js.Dict.empty()
          Js.Dict.set(dict_data, "fire", Js.Json.Array([]))
          Js.Dict.set(dict_data, "ice", Js.Json.Array([]))
          Js.Dict.set(dict_data, "player", Js.Json.Array([
            Js.Json.number(0.0),
            Js.Json.number(0.0),
          ]))
          dict_data
        }
        setFire(_ => dict_out->Js.Dict.unsafeGet("fire")->decode_obstacles)
        setIce(_ => dict_out->Js.Dict.unsafeGet("ice")->decode_obstacles)
      }

      fetchCall()
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

  <div className="grid">
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
