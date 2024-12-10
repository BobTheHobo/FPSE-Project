// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Grid from "./Grid.res.mjs";
import * as React from "react";
import * as Placeholder from "./Placeholder.res.mjs";
import * as GameConfigForm from "./GameConfigForm.res.mjs";
import * as JsxRuntime from "react/jsx-runtime";

var defaultOptions = {
  b: 3,
  s1: 2,
  s2: 3
};

function App(props) {
  var match = React.useState(function () {
        return defaultOptions;
      });
  var match$1 = React.useState(function () {
        return defaultOptions;
      });
  var match$2 = React.useState(function () {
        return defaultOptions;
      });
  var match$3 = React.useState(function () {
        return false;
      });
  var grid = match$3[0] ? JsxRuntime.jsx(Grid.make, {}) : JsxRuntime.jsx(Placeholder.make, {});
  return JsxRuntime.jsxs("main", {
              children: [
                JsxRuntime.jsxs("div", {
                      children: [
                        JsxRuntime.jsxs("div", {
                              children: [
                                JsxRuntime.jsx("h1", {
                                      children: "Can you beat the Game of Life?",
                                      className: "text-white text-xl"
                                    }),
                                JsxRuntime.jsx("h3", {
                                      children: "You: 0",
                                      className: "text-white"
                                    }),
                                JsxRuntime.jsx("h3", {
                                      children: "Computer: 0",
                                      className: "text-white"
                                    })
                              ],
                              className: "flex flex-col gap-1 border-b border-gray-400 px-2"
                            }),
                        JsxRuntime.jsxs("div", {
                              children: [
                                JsxRuntime.jsx(GameConfigForm.make, {
                                      value: match[0],
                                      onValueChange: match[1],
                                      cellType: "Fire"
                                    }),
                                JsxRuntime.jsx(GameConfigForm.make, {
                                      value: match$1[0],
                                      onValueChange: match$1[1],
                                      cellType: "Ice"
                                    }),
                                JsxRuntime.jsx(GameConfigForm.make, {
                                      value: match$2[0],
                                      onValueChange: match$2[1],
                                      cellType: "Water"
                                    }),
                                JsxRuntime.jsx("button", {
                                      children: "Begin game",
                                      className: "mt-4 border border-gray-600 rounded-md h-12 px-4 text-white bg-blue-700"
                                    })
                              ],
                              className: "p-4"
                            })
                      ],
                      className: "col-span-2 border-r border-gray-400 p-2"
                    }),
                JsxRuntime.jsx("div", {
                      children: grid,
                      className: "col-span-5 flex justify-center mt-16"
                    })
              ],
              className: "bg-gray-900 grid grid-cols-7 min-h-svh"
            });
}

var make = App;

export {
  make ,
}
/* Grid Not a pure module */
