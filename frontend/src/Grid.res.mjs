// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Js_json from "rescript/lib/es6/js_json.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as JsxRuntime from "react/jsx-runtime";

function getColorClass(state) {
  switch (state) {
    case 0 :
        return "traversable";
    case 1 :
        return "goal";
    case 2 :
        return "player";
    case 3 :
        return "fire";
    case 4 :
        return "ice";
    default:
      return "default";
  }
}

function decodePosition(array) {
  var match = Js_json.decodeArray(array);
  if (match === undefined) {
    return [
            0,
            0
          ];
  }
  if (match.length !== 2) {
    return [
            0,
            0
          ];
  }
  var a = match[0];
  var b = match[1];
  var match$1 = Js_json.decodeNumber(a);
  var match$2 = Js_json.decodeNumber(b);
  if (match$1 !== undefined && match$2 !== undefined) {
    return [
            match$1 | 0,
            match$2 | 0
          ];
  } else {
    return [
            0,
            0
          ];
  }
}

function decode_obstacles(obstacle_array) {
  var arrayData = Js_json.decodeArray(obstacle_array);
  if (arrayData !== undefined) {
    return Belt_Array.keepMap(Belt_Array.map(arrayData, decodePosition), (function (pair) {
                  return pair;
                }));
  } else {
    return [];
  }
}

function Grid(props) {
  var match = React.useState(function () {
        return [
                0,
                0
              ];
      });
  var setPosition = match[1];
  var position = match[0];
  var match$1 = React.useState(function () {
        return [];
      });
  var setFire = match$1[1];
  var fire = match$1[0];
  var match$2 = React.useState(function () {
        return [];
      });
  var setIce = match$2[1];
  var ice = match$2[0];
  var match$3 = React.useState(function () {
        return true;
      });
  var setIsValidMove = match$3[1];
  var isValidMove = match$3[0];
  var maxX = 9;
  var maxY = 9;
  var grid = Belt_Array.map(Belt_Array.make(10, undefined), (function () {
          return Belt_Array.make(10, 0);
        }));
  var fetchObstacles = function () {
    if (!isValidMove) {
      return ;
    }
    console.log("HEREEEEE");
    var fetchCall = async function () {
      var response = await fetch("http://localhost:8080/get_obstacles", {
            method: "POST",
            body: Caml_option.some(Belt_Option.getExn(JSON.stringify({
                          fire: fire,
                          ice: ice,
                          player: position
                        }))),
            headers: Caml_option.some(new Headers({
                      "Content-type": "application/json"
                    }))
          });
      var json_out = await response.json();
      console.log(json_out);
<<<<<<< HEAD
      var dict_data = Js_json.decodeObject(json_out);
      var dict_out;
      if (dict_data !== undefined) {
        dict_out = dict_data;
=======
      var objectData = Js_json.decodeObject(json_out);
      var json_outd;
      if (objectData !== undefined) {
        var isDeadEncoded = objectData["is_dead"];
        var isDead = getBool(isDeadEncoded);
        if (isDead) {
          setPosition(function (param) {
                return [
                        0,
                        0
                      ];
              });
          json_outd = [];
        } else {
          var arr = Js_json.decodeArray(objectData["obstacles"]);
          json_outd = arr !== undefined ? Belt_Array.keepMap(Belt_Array.map(arr, (function (item) {
                        var match = Js_json.decodeArray(item);
                        if (match === undefined) {
                          return ;
                        }
                        if (match.length !== 2) {
                          return ;
                        }
                        var a = match[0];
                        var b = match[1];
                        var match$1 = Js_json.decodeNumber(a);
                        var match$2 = Js_json.decodeNumber(b);
                        if (match$1 !== undefined && match$2 !== undefined) {
                          return [
                                  match$1 | 0,
                                  match$2 | 0
                                ];
                        }
                        
                      })), (function (pair) {
                    return pair;
                  })) : [];
        }
>>>>>>> 70b31cf (fix init_obstacles to return false as dead state)
      } else {
        var dict_data$1 = {};
        dict_data$1["fire"] = [];
        dict_data$1["ice"] = [];
        dict_data$1["player"] = [
          0.0,
          0.0
        ];
        dict_out = dict_data$1;
      }
      setFire(function (param) {
            return decode_obstacles(dict_out["fire"]);
          });
      setIce(function (param) {
            return decode_obstacles(dict_out["ice"]);
          });
      if (Caml_obj.equal(decodePosition(dict_out["player"]), [
              0,
              0
            ])) {
        return setPosition(function (param) {
                    return [
                            0,
                            0
                          ];
                  });
      }
      
    };
    fetchCall();
  };
  var onKeyDown = function (evt) {
    var key = evt.key;
    console.log(position);
    switch (key) {
      case "ArrowDown" :
          setPosition(function (param) {
                var y = param[1];
                var x = param[0];
                if ((x + 1 | 0) <= maxX) {
                  setIsValidMove(function (param) {
                        return true;
                      });
                  return [
                          Math.min(x + 1 | 0, maxX),
                          y
                        ];
                } else {
                  setIsValidMove(function (param) {
                        return false;
                      });
                  return [
                          x,
                          y
                        ];
                }
              });
          evt.preventDefault();
          return ;
      case "ArrowLeft" :
          setPosition(function (param) {
                var y = param[1];
                var x = param[0];
                if ((y - 1 | 0) >= 0) {
                  setIsValidMove(function (param) {
                        return true;
                      });
                  return [
                          x,
                          Math.max(y - 1 | 0, 0)
                        ];
                } else {
                  setIsValidMove(function (param) {
                        return false;
                      });
                  return [
                          x,
                          y
                        ];
                }
              });
          evt.preventDefault();
          return ;
      case "ArrowRight" :
          setPosition(function (param) {
                var y = param[1];
                var x = param[0];
                if ((y + 1 | 0) <= maxY) {
                  setIsValidMove(function (param) {
                        return true;
                      });
                  return [
                          x,
                          Math.min(y + 1 | 0, maxY)
                        ];
                } else {
                  setIsValidMove(function (param) {
                        return false;
                      });
                  return [
                          x,
                          y
                        ];
                }
              });
          evt.preventDefault();
          return ;
      case "ArrowUp" :
          setPosition(function (param) {
                var y = param[1];
                var x = param[0];
                if ((x - 1 | 0) >= 0) {
                  setIsValidMove(function (param) {
                        return true;
                      });
                  return [
                          Math.max(x - 1 | 0, 0),
                          y
                        ];
                } else {
                  setIsValidMove(function (param) {
                        return false;
                      });
                  return [
                          x,
                          y
                        ];
                }
              });
          evt.preventDefault();
          return ;
      default:
        return ;
    }
  };
  React.useEffect((function () {
          fetchObstacles();
        }), [position]);
  React.useEffect((function () {
          document.addEventListener("keyup", onKeyDown);
          return (function () {
                    document.removeEventListener("keyup", onKeyDown);
                  });
        }), []);
  Belt_Array.setExn(Belt_Array.getExn(grid, maxX), maxY, 1);
  Belt_Array.setExn(Belt_Array.getExn(grid, position[0]), position[1], 2);
  Belt_Array.forEach(fire, (function (pos) {
          Belt_Array.setExn(Belt_Array.getExn(grid, pos[0]), pos[1], 3);
        }));
  Belt_Array.forEach(ice, (function (pos) {
          Belt_Array.setExn(Belt_Array.getExn(grid, pos[0]), pos[1], 4);
        }));
  return JsxRuntime.jsx("div", {
              children: grid.map(function (row, y) {
                    return JsxRuntime.jsx("div", {
                                children: row.map(function (state, x) {
                                      return JsxRuntime.jsx("div", {
                                                  className: "cell " + getColorClass(state)
                                                }, String(x));
                                    }),
                                className: "row"
                              }, String(y));
                  }),
              className: "grid"
            });
}

var make = Grid;

export {
  make ,
}
/* react Not a pure module */
