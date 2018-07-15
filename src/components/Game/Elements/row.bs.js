// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as Cn from "re-classnames/src/Cn.bs.js";
import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as React from "react";
import * as Random from "bs-platform/lib/es6/random.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Helpers$ReasonScripts from "../../../utils/Helpers.bs.js";
import * as Caml_builtin_exceptions from "bs-platform/lib/es6/caml_builtin_exceptions.js";

var rowComponent = ReasonReact.statelessComponent("Row");

function mapRowType(x) {
  switch (x) {
    case "0" : 
        return /* Top */0;
    case "1" : 
        return /* Middle */1;
    case "2" : 
        return /* Bottom */2;
    default:
      throw [
            Caml_builtin_exceptions.match_failure,
            [
              "Row.re",
              13,
              2
            ]
          ];
  }
}

function getRowNumber(cellNo) {
  return Math.floor(cellNo / 24 | 0) | 0;
}

var randomiseTile = Random.init(4);

function wrapRowClass(cellNo, lane) {
  var rowNumber = getRowNumber(cellNo);
  var rowType = mapRowType(String(rowNumber));
  var cellModifier = [""];
  if (cellNo === 6) {
    cellModifier[0] = "stairs";
  }
  if (cellNo === 16) {
    cellModifier[0] = "stairs";
  }
  if (lane === "1") {
    if (cellNo === 2) {
      cellModifier[0] = "blue-fountain";
    }
    
  }
  if (lane === "2") {
    if (cellNo === 18) {
      cellModifier[0] = "blue-fountain";
    }
    
  }
  if (lane === "3") {
    if (cellNo === 10) {
      cellModifier[0] = "blue-fountain";
    }
    
  }
  var row;
  switch (rowType) {
    case 0 : 
        row = "top";
        break;
    case 1 : 
        row = "middle";
        break;
    case 2 : 
        row = "bottom";
        break;
    
  }
  return "row-" + (String(row) + (" " + (String(cellModifier) + "")));
}

function make(lane, _) {
  return /* record */[
          /* debugName */rowComponent[/* debugName */0],
          /* reactClassInternal */rowComponent[/* reactClassInternal */1],
          /* handedOffState */rowComponent[/* handedOffState */2],
          /* willReceiveProps */rowComponent[/* willReceiveProps */3],
          /* didMount */rowComponent[/* didMount */4],
          /* didUpdate */rowComponent[/* didUpdate */5],
          /* willUnmount */rowComponent[/* willUnmount */6],
          /* willUpdate */rowComponent[/* willUpdate */7],
          /* shouldUpdate */rowComponent[/* shouldUpdate */8],
          /* render */(function () {
              var totalCells = 72;
              var cells = Helpers$ReasonScripts.range(0, totalCells);
              return React.createElement("div", {
                          className: "row"
                        }, React.createElement("div", {
                              className: "spriteGrid",
                              style: {
                                gridTemplateColumns: "repeat(" + (String(24) + ", 1fr)")
                              }
                            }, $$Array.of_list(List.map((function (i) {
                                        return React.createElement("div", {
                                                    key: "row-" + (String(i) + ""),
                                                    className: Cn.make(/* :: */[
                                                          "cell",
                                                          /* :: */[
                                                            wrapRowClass(i, lane),
                                                            /* [] */0
                                                          ]
                                                        ])
                                                  });
                                      }), cells))));
            }),
          /* initialState */rowComponent[/* initialState */10],
          /* retainedProps */rowComponent[/* retainedProps */11],
          /* reducer */rowComponent[/* reducer */12],
          /* subscriptions */rowComponent[/* subscriptions */13],
          /* jsElementWrapped */rowComponent[/* jsElementWrapped */14]
        ];
}

var _CELLS_PER_ROW = 24;

var _ROWS_PER_GRID = 3;

var _VIEWPORT_WIDTH = 1297;

export {
  rowComponent ,
  _CELLS_PER_ROW ,
  _ROWS_PER_GRID ,
  _VIEWPORT_WIDTH ,
  mapRowType ,
  getRowNumber ,
  randomiseTile ,
  wrapRowClass ,
  make ,
  
}
/* rowComponent Not a pure module */
