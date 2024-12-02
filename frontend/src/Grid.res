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
  let gridSize = 10
  let maxX = gridSize - 1
  let maxY = gridSize - 1

  let grid = make(gridSize, ())->map(() => make(gridSize, 0))

  let fetchObstacles = () => {
    // setObstacles(old_obstacles => async () => {

    // })
    let fetchCall = async () => {
      Js.log(obstacles)
      let response = await fetch(
        "http://localhost:8080/get_obstacles",
        {
          method: #POST,
          body: {
            "obstacles": obstacles,
            "player": position,
          }->Js.Json.stringifyAny->Belt.Option.getExn->Body.string,
          headers: Headers.fromObject({
            "Content-type": "application/json",
          }),
        },
      )
      
      let getBool = encoded => switch Js.Json.decodeBoolean(encoded) {
        | Some(value) => value
        | None => true
      }

      let json_out = await response->Response.json
      let json_outd = switch Js.Json.decodeObject(json_out) {
      | Some(objectData) =>
          let isDeadEncoded = Js.Dict.unsafeGet(objectData, "is_dead")
          let isDead = getBool(isDeadEncoded)
          if (isDead) {
            setPosition(_ => (0, 0))
            []
          } else {
            switch Js.Json.decodeArray(Js.Dict.unsafeGet(objectData, "obstacles")) {
              | Some(arr) => arr->Belt.Array.map(item =>
                switch Js.Json.decodeArray(item) {
                | Some([a, b]) =>
                    switch (Js.Json.decodeNumber(a), Js.Json.decodeNumber(b)) {
                    | (Some(a), Some(b)) => Some((int_of_float(a), int_of_float(b)))
                    | _ => None
                    }
                | _ => None
                })
            -> Belt.Array.keepMap(pair => pair)
              | None => []
            }
          }
      | None => []
      }
      Js.log(json_outd)
      setObstacles(_ => json_outd)
    }

    fetchCall()
    |> ignore
  }

  let onKeyDown = evt => {
    let key = ReactEvent.Keyboard.key(evt)

    switch key {
    | "ArrowLeft" =>
      Js.log("Move left")
      setPosition(((x, y)) => (x, Js.Math.max_int(y - 1, 0)))
      ReactEvent.Keyboard.preventDefault(evt)
    | "ArrowRight" =>
      Js.log("Move right")
      setPosition(((x, y)) => (x, Js.Math.min_int(y + 1, maxY)))
      ReactEvent.Keyboard.preventDefault(evt)
    | "ArrowUp" =>
      Js.log("Move up")
      setPosition(((x, y)) => (Js.Math.max_int(x - 1, 0), y))
      ReactEvent.Keyboard.preventDefault(evt)
    | "ArrowDown" =>
      Js.log("Move Down")
      setPosition(((x, y)) => (Js.Math.min_int(x + 1, maxX), y))
      ReactEvent.Keyboard.preventDefault(evt)
    | _ => ()
    }
    // fetchObstacles()
  }

  React.useEffect(() => {
    fetchObstacles()
    None
  }, [position])

  React.useEffect(() => {
    // fetchObstacles()
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
