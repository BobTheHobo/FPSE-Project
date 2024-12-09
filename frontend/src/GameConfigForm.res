open React
open Js.Json

type game_params = {
  b: int,
  s1: int,
  s2: int,
}

@react.component
let make = (~value: (game_params), ~onValueChange: ((game_params) => game_params) => unit) => {
  <div>
    <div>
      <h1 className="text-lg"> {"Set Fire cell params"->React.string} </h1>
      <div>
        <p className="text-sm"> {"Spawn neighbor count"->React.string} </p>
        <input type_="number" min="1" max="25" value={Js.Int.toString(value.b)} onChange={event => {
          let target = JsxEvent.Form.target(event)
          let value : string = target["value"]
          let converted : option<int> = Belt.Int.fromString(value)
          switch (converted) {
            | Some (integer) => onValueChange(params => { ...params, b: integer })
            | _ => ()
          }
        }}/>
      </div>
      <div>
        <p className="text-sm"> {"Minimum survival count"->React.string} </p>
        <input type_="number" min="1" max="25" value={Js.Int.toString(value.s1)} onChange={event => {
          let target = JsxEvent.Form.target(event)
          let value : string = target["value"]
          let converted : option<int> = Belt.Int.fromString(value)
          switch (converted) {
            | Some (integer) => onValueChange(params => { ...params, s1: integer })
            | _ => ()
          }
        }}/>
      </div>
      <div>
        <p className="text-sm"> {"Maximum survival count"->React.string} </p>
        <input type_="number" min="1" max="25" value={Js.Int.toString(value.s2)} onChange={event => {
          let target = JsxEvent.Form.target(event)
          let value : string = target["value"]
          let converted : option<int> = Belt.Int.fromString(value)
          switch (converted) {
            | Some (integer) => onValueChange(params => { ...params, s2: integer })
            | _ => ()
          }
        }}/>
      </div>
    </div>
  </div>
}
