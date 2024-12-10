// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as JsxRuntime from "react/jsx-runtime";

function GameConfigForm(props) {
  var onValueChange = props.onValueChange;
  var value = props.value;
  return JsxRuntime.jsx("div", {
              children: JsxRuntime.jsxs("div", {
                    children: [
                      JsxRuntime.jsx("h1", {
                            children: "Set " + props.cellType + " params",
                            className: "text-lg text-white"
                          }),
                      JsxRuntime.jsxs("div", {
                            children: [
                              JsxRuntime.jsx("p", {
                                    children: "Spawn neighbor count",
                                    className: "text-sm text-white"
                                  }),
                              JsxRuntime.jsx("input", {
                                    max: "25",
                                    min: "1",
                                    type: "number",
                                    value: value.b.toString(),
                                    onChange: (function ($$event) {
                                        var target = $$event.target;
                                        var value = target.value;
                                        var converted = Belt_Int.fromString(value);
                                        if (converted !== undefined) {
                                          return onValueChange(function (params) {
                                                      return {
                                                              b: converted,
                                                              s1: params.s1,
                                                              s2: params.s2
                                                            };
                                                    });
                                        }
                                        
                                      })
                                  })
                            ]
                          }),
                      JsxRuntime.jsxs("div", {
                            children: [
                              JsxRuntime.jsx("p", {
                                    children: "Minimum survival count",
                                    className: "text-sm text-white"
                                  }),
                              JsxRuntime.jsx("input", {
                                    max: "25",
                                    min: "1",
                                    type: "number",
                                    value: value.s1.toString(),
                                    onChange: (function ($$event) {
                                        var target = $$event.target;
                                        var value = target.value;
                                        var converted = Belt_Int.fromString(value);
                                        if (converted !== undefined) {
                                          return onValueChange(function (params) {
                                                      return {
                                                              b: params.b,
                                                              s1: converted,
                                                              s2: params.s2
                                                            };
                                                    });
                                        }
                                        
                                      })
                                  })
                            ]
                          }),
                      JsxRuntime.jsxs("div", {
                            children: [
                              JsxRuntime.jsx("p", {
                                    children: "Maximum survival count",
                                    className: "text-sm text-white"
                                  }),
                              JsxRuntime.jsx("input", {
                                    max: "25",
                                    min: "1",
                                    type: "number",
                                    value: value.s2.toString(),
                                    onChange: (function ($$event) {
                                        var target = $$event.target;
                                        var value = target.value;
                                        var converted = Belt_Int.fromString(value);
                                        if (converted !== undefined) {
                                          return onValueChange(function (params) {
                                                      return {
                                                              b: params.b,
                                                              s1: params.s1,
                                                              s2: converted
                                                            };
                                                    });
                                        }
                                        
                                      })
                                  })
                            ]
                          })
                    ]
                  })
            });
}

var make = GameConfigForm;

export {
  make ,
}
/* react/jsx-runtime Not a pure module */