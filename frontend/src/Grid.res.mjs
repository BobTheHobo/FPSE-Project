// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Js_json from "rescript/lib/es6/js_json.js";
import * as Js_math from "rescript/lib/es6/js_math.js";
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

function encode_request_body(game_id, player_position) {
  return Belt_Option.getExn(JSON.stringify({
                  game_id: game_id,
                  player_position: player_position
                }));
}

function decode_response_body(payload) {
  var response_body = Js_json.decodeObject(payload);
  if (response_body !== undefined) {
    return response_body;
  } else {
    return {};
  }
}

function number_to_int(number) {
  var a = Js_json.decodeNumber(number);
  if (a !== undefined) {
    return a | 0;
  } else {
    return -1;
  }
}

function decodeCoordinate(coordinate) {
  var obj = Js_json.decodeObject(coordinate);
  if (obj === undefined) {
    return {
            x: -1,
            y: -1
          };
  }
  var match = Js_dict.get(obj, "x");
  var match$1 = Js_dict.get(obj, "y");
  if (match !== undefined && !(!Array.isArray(match) && (match === null || typeof match !== "object") && typeof match !== "number" && typeof match !== "string" && typeof match !== "boolean" || !(typeof match === "number" && match$1 !== undefined && !(!Array.isArray(match$1) && (match$1 === null || typeof match$1 !== "object") && typeof match$1 !== "number" && typeof match$1 !== "string" && typeof match$1 !== "boolean" || typeof match$1 !== "number")))) {
    return {
            x: Js_math.floor(match),
            y: Js_math.floor(match$1)
          };
  } else {
    return {
            x: -1,
            y: -1
          };
  }
}

function decodeCellTypes(cellTypes) {
  var array = Js_json.decodeArray(cellTypes);
  if (array !== undefined) {
    return Belt_Array.keepMap(array, Js_json.decodeString);
  } else {
    return [];
  }
}

function obstaclesToTuples(obstacles, cellType) {
  var array = Js_json.decodeArray(obstacles);
  if (array !== undefined) {
    return Belt_Array.map(array, (function (a) {
                    var obj = Js_json.decodeObject(a);
                    if (obj === undefined) {
                      return [
                              -1,
                              -1
                            ];
                    }
                    var coordinate = obj["coordinate"];
                    var match = decodeCoordinate(coordinate);
                    var cellTypes = obj["cell_types"];
                    var foundIndex = decodeCellTypes(cellTypes).findIndex(function (cell) {
                          return cell === cellType;
                        });
                    if (foundIndex > -1) {
                      return [
                              match.x,
                              match.y
                            ];
                    } else {
                      return [
                              -1,
                              -1
                            ];
                    }
                  })).filter(function (param) {
                if (param[0] !== -1) {
                  return param[1] !== -1;
                } else {
                  return false;
                }
              });
  } else {
    return [];
  }
}

function tupleToPosition(param) {
  return {
          x: param[0],
          y: param[1]
        };
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
  var match$2 = React.useState(function () {
        return [];
      });
  var setIce = match$2[1];
  var match$3 = React.useState(function () {
        return true;
      });
  var setIsValidMove = match$3[1];
  var isValidMove = match$3[0];
  var match$4 = React.useState(function () {
        return -1;
      });
  var setGameId = match$4[1];
  var gameId = match$4[0];
  var maxX = 14;
  var maxY = 14;
  var grid = Belt_Array.map(Belt_Array.make(15, undefined), (function () {
          return Belt_Array.make(15, 0);
        }));
  var fetchObstacles = function () {
    if (!isValidMove) {
      return ;
    }
    console.log("HEREEEEE");
    var gameCall = async function () {
      var response = await fetch("http://localhost:8080/game", {
            method: "POST",
            body: Caml_option.some(encode_request_body(gameId, tupleToPosition(position))),
            headers: Caml_option.some(new Headers({
                      "Content-Type": "application/json"
                    }))
          });
      var payload = await response.json();
      var decoded = decode_response_body(payload);
      var gameIdNum = decoded["game_id"];
      var obstaclesRaw = decoded["obstacles"];
      var fireTuples = obstaclesToTuples(obstaclesRaw, "Fire");
      var iceTuples = obstaclesToTuples(obstaclesRaw, "Ice");
      console.log("Fire tuples");
      console.log(fireTuples);
      console.log("Ice tuples");
      console.log(iceTuples);
      var asInt = number_to_int(gameIdNum);
      setGameId(function (param) {
            return asInt;
          });
      if (fireTuples.length > 0) {
        setFire(function (param) {
              return fireTuples;
            });
      }
      if (iceTuples.length > 0) {
        return setIce(function (param) {
                    return iceTuples;
                  });
      }
      
    };
    gameCall();
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
  console.log(gameId);
  Belt_Array.setExn(Belt_Array.getExn(grid, maxX), maxY, 1);
  Belt_Array.setExn(Belt_Array.getExn(grid, position[0]), position[1], 2);
  Belt_Array.forEach(match$1[0], (function (pos) {
          Belt_Array.setExn(Belt_Array.getExn(grid, pos[0]), pos[1], 3);
        }));
  Belt_Array.forEach(match$2[0], (function (pos) {
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
