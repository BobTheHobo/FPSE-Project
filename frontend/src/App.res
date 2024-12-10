open React
open Fetch
open Js.Json

let defaultOptions: GameConfigForm.game_params = {
  b: 3,
  s1: 2,
  s2: 3,
}

// let cookieExists = (name: string): bool => {
//   let cookies = Dom.document->Dom.Document["cookie"]
//   switch cookies {
//   | None => false
//   | Some(cookieString) =>
//     cookieString
//     ->Js.String.split(";")
//     ->Array.exists(cookie => {
//       cookie
//       ->Js.String.trim
//       ->Js.String.startsWith(name ++ "=")
//     })
//   }
// }

@react.component
let make = () => {
  let (fireParams, setFireParams) = useState(_ => defaultOptions)
  let (iceParams, setIceParams) = useState(_ => defaultOptions)
  let (waterParams, setWaterParams) = useState(_ => defaultOptions)
  let (hasGameId, setHasGameId) = useState(_ => false)

  let grid = if hasGameId {
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
      Js.log(payload)
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
