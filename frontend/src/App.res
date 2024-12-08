@react.component
let make = () => {
  // let (count, setCount) = React.useState(() => 0)

  <div className="p-6 flex flex-col justify-center items-center bg-gray-900 h-screen">
    <h1 className="text-white text-6xl">{"Win at life (literally)"->React.string}</h1>
    <h3 className="text-white">{"You: 0"->React.string}</h3>
    <h3 className="text-white mb-24">{"Computer: 0"->React.string}</h3>
    // <h1 className="text-3xl font-semibold"> {"What is this about?"->React.string} </h1>
    // <p>
    //   {React.string("This is a simple template for a Vite project using ReScript & Tailwind CSS.")}
    // </p>
    // <h2 className="text-2xl font-semibold mt-5"> {React.string("Fast Refresh Test")} </h2>
    // <Button onClick={_ => setCount(count => count + 1)}>
    //   {React.string(`count is ${count->Int.toString}`)}
    // </Button>
    // <p>
    //   {React.string("Edit ")}
    //   <code> {React.string("src/App.res")} </code>
    //   {React.string(" and save to test Fast Refresh.")}
    // </p>
    <Grid></Grid>
  </div>
}
