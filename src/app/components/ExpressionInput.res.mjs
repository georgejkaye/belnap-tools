// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Belnap from "../theory/Belnap.res.mjs";
import * as Mathjax from "../bindings/Mathjax.res.mjs";
import * as MoreReact from "../bindings/MoreReact.res.mjs";
import * as Expression from "../theory/Expression.res.mjs";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as JsxRuntime from "react/jsx-runtime";

function ExpressionInput$InputBox(props) {
  var setExpression = props.setExpression;
  var match = React.useState(function () {
        return "";
      });
  var setExpressionText = match[1];
  var expressionText = match[0];
  var onChangeExpressionInput = function (e) {
    setExpressionText(e.target.value);
  };
  var parseExpression = function (e) {
    var expression = Expression.parseExpression(expressionText);
    if (expression.TAG !== "Succ") {
      return ;
    }
    var exp = expression._0;
    setExpression(function (param) {
          return Caml_option.some(exp);
        });
  };
  var onKeyDownExpressionInput = function (e) {
    if (e.key === "Enter") {
      return parseExpression(e);
    }
    
  };
  var onClickParseButton = function (e) {
    parseExpression(e);
  };
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx("input", {
                      className: "w-full rounded-lg border p-2",
                      placeholder: "Type expression",
                      type: "text",
                      value: expressionText,
                      onKeyDown: onKeyDownExpressionInput,
                      onChange: onChangeExpressionInput
                    }),
                JsxRuntime.jsx("button", {
                      children: "Parse",
                      className: "rounded-lg border p-2",
                      onClick: onClickParseButton
                    })
              ],
              className: "flex flex-row items-center gap-2"
            });
}

var InputBox = {
  make: ExpressionInput$InputBox
};

function ExpressionInput$CheatSheetItem(props) {
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx("div", {
                      children: props.input,
                      className: "rounded-lg px-2 bg-gray-100 font-mono"
                    }),
                JsxRuntime.jsx("div", {
                      children: props.meaning,
                      className: ""
                    })
              ],
              className: "flex flex-row my-2 gap-2"
            });
}

var CheatSheetItem = {
  make: ExpressionInput$CheatSheetItem
};

function ExpressionInput$CheatSheet(props) {
  var match = React.useState(function () {
        return false;
      });
  var setIsOpen = match[1];
  var isOpen = match[0];
  var onClickCheatSheetButton = function (param) {
    setIsOpen(function (old) {
          return !old;
        });
  };
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx("button", {
                      children: (
                        isOpen ? "Hide" : "Show"
                      ) + " cheat sheet",
                      className: "w-full text-left font-bold",
                      onClick: onClickCheatSheetButton
                    }),
                isOpen ? JsxRuntime.jsxs("div", {
                        children: [
                          JsxRuntime.jsxs("div", {
                                children: [
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "n",
                                        meaning: "⊥ value ('none')"
                                      }),
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "b",
                                        meaning: "f value ('false')"
                                      }),
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "b",
                                        meaning: "t value ('true')"
                                      }),
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "b",
                                        meaning: "⊤ value ('both')"
                                      })
                                ],
                                className: "flex flex-col"
                              }),
                          JsxRuntime.jsxs("div", {
                                children: [
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "v0",
                                        meaning: "variable 0"
                                      }),
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "v1",
                                        meaning: "variable 1"
                                      }),
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "v2",
                                        meaning: "variable 2"
                                      }),
                                  JsxRuntime.jsx("div", {
                                        children: "...",
                                        className: "self-center"
                                      })
                                ],
                                className: "flex flex-col"
                              }),
                          JsxRuntime.jsxs("div", {
                                children: [
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "<exp> && <exp>",
                                        meaning: "AND"
                                      }),
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "<exp> || <exp>",
                                        meaning: "OR"
                                      }),
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "<exp> VV <exp>",
                                        meaning: "join"
                                      }),
                                  JsxRuntime.jsx(ExpressionInput$CheatSheetItem, {
                                        input: "¬<exp>",
                                        meaning: "NOT"
                                      })
                                ],
                                className: "flex flex-col"
                              })
                        ],
                        className: "flex flex-row gap-5"
                      }) : ""
              ],
              className: "flex flex-col border rounded-lg items-left p-4 gap-4"
            });
}

var CheatSheet = {
  make: ExpressionInput$CheatSheet
};

function ExpressionInput$ExpressionDisplay(props) {
  return JsxRuntime.jsx("div", {
              children: JsxRuntime.jsx(Mathjax.make, {
                    children: Mathjax.inline(Expression.latex_of_expression(props.expression)),
                    inline: true,
                    dynamic: true
                  }),
              className: "p-4 bg-green-800 text-yellow-300 rounded-lg"
            });
}

var ExpressionDisplay = {
  make: ExpressionInput$ExpressionDisplay
};

function ExpressionInput$TableDisplay(props) {
  var expression = props.expression;
  var numberOfInputs = Expression.highestVariable(expression) + 1 | 0;
  var inputSignals = Belnap.enumerate_inputs(numberOfInputs);
  var expressionFunction = Expression.functionOfExpression(expression);
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx("div", {
                      children: MoreReact.map(inputSignals, (function (signal) {
                              return JsxRuntime.jsx("div", {
                                          children: Belnap.string_of_value_array(signal),
                                          className: "font-mono"
                                        });
                            })),
                      className: "border-r pr-4 flex flex-col gap-2"
                    }),
                JsxRuntime.jsx("div", {
                      children: MoreReact.map(inputSignals, (function (signal) {
                              return JsxRuntime.jsx("div", {
                                          children: Belnap.string_of_value(expressionFunction(signal)),
                                          className: "font-mono"
                                        });
                            })),
                      className: "px-4 flex flex-col gap-2"
                    })
              ],
              className: "flex flex-row"
            });
}

var TableDisplay = {
  make: ExpressionInput$TableDisplay
};

function ExpressionInput(props) {
  var match = React.useState(function () {
        
      });
  var expression = match[0];
  var tmp;
  if (expression !== undefined) {
    var expression$1 = Caml_option.valFromOption(expression);
    tmp = JsxRuntime.jsxs("div", {
          children: [
            JsxRuntime.jsx(ExpressionInput$ExpressionDisplay, {
                  expression: expression$1
                }),
            JsxRuntime.jsx(ExpressionInput$TableDisplay, {
                  expression: expression$1
                })
          ],
          className: "flex flex-col gap-4"
        });
  } else {
    tmp = "";
  }
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx(ExpressionInput$InputBox, {
                      expression: expression,
                      setExpression: match[1]
                    }),
                JsxRuntime.jsx(ExpressionInput$CheatSheet, {}),
                tmp
              ],
              className: "flex flex-col gap-4"
            });
}

var make = ExpressionInput;

export {
  InputBox ,
  CheatSheetItem ,
  CheatSheet ,
  ExpressionDisplay ,
  TableDisplay ,
  make ,
}
/* react Not a pure module */
