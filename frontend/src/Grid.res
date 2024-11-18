open Belt.Array
open React
open Fetch


@val external addEventListener: (string, ReactEvent.Keyboard.t => unit) => unit = "document.addEventListener"
@val external removeEventListener: (string, ReactEvent.Keyboard.t => unit) => unit = "document.removeEventListener"

let getColorClass = (state: int) => {
  switch state {
  | 0 => "traversable"
  | 1 => "player"
  | 2 => "obstacle"
  | 3 => "goal"
  | _ => "default"
  }
}

@react.component
let make = () => {
  let (position, setPosition) = useState(() => (0, 0))
  let (obstacles, setObstacles) = useState(() => [])
  let (isValidMove, setIsValidMove) = useState(() => true)
  let gridSize = 10
  let maxX = gridSize - 1
  let maxY = gridSize - 1

  let grid = make(gridSize, ())->map(() => make(gridSize, 0))

  let fetchObstacles = () => {
    if (isValidMove) {
      let fetchCall = async () => {
        let response = await fetch(
          "http://localhost:8080/get_obstacles",
          {
            method: #POST,
            body: {"obstacles": obstacles}->Js.Json.stringifyAny->Belt.Option.getExn->Body.string,
            headers: Headers.fromObject({
              "Content-type": "application/json",
            }),
          },
        )

        let json_out = await response->Response.json
        let json_outd = switch Js.Json.decodeArray(json_out) {
        | Some(arrayData) =>
            arrayData
            -> Belt.Array.map(item =>
                switch Js.Json.decodeArray(item) {
                | Some([a, b]) =>
                    switch (Js.Json.decodeNumber(a), Js.Json.decodeNumber(b)) {
                    | (Some(a), Some(b)) => Some((int_of_float(a), int_of_float(b)))
                    | _ => None
                    }
                | _ => None
                })
            -> Belt.Array.keepMap(pair => pair)
        | None =>
            []
        }
        setObstacles(_ => json_outd)
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
  grid->getExn(x)->setExn(y, 1)
  grid->getExn(maxX)->setExn(maxY, 3)
  obstacles->Belt.Array.forEach(pos => {
        let (x, y) = pos
        grid->getExn(x)->setExn(y, 2)})

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
