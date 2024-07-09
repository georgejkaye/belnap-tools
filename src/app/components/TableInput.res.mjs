// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Belnap from "../theory/Belnap.res.mjs";
import * as Mathjax from "../bindings/Mathjax.res.mjs";
import * as Core__Int from "@rescript/core/src/Core__Int.res.mjs";
import * as MoreReact from "../bindings/MoreReact.res.mjs";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Expression from "../theory/Expression.res.mjs";
import * as TruthTable from "./TruthTable.res.mjs";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";
import * as JsxRuntime from "react/jsx-runtime";
import * as BetterReactMathjax from "better-react-mathjax";

function onChangeValue(setValue, ev) {
  var value = Core__Int.fromString(ev.target.value, undefined);
  if (value !== undefined) {
    return setValue(function (param) {
                return value;
              });
  } else {
    return setValue(function (param) {
                
              });
  }
}

function TableInput$InterfaceInput(props) {
  var setValue = props.setValue;
  var value = props.value;
  var label = props.label;
  var id = label.toLowerCase();
  return JsxRuntime.jsxs(JsxRuntime.Fragment, {
              children: [
                JsxRuntime.jsx("label", {
                      children: label,
                      className: "py-2",
                      htmlFor: id
                    }),
                JsxRuntime.jsx("input", {
                      className: "w-20 border p-2 rounded-lg",
                      id: id,
                      min: "0",
                      type: "number",
                      value: value !== undefined ? value.toString() : "",
                      onChange: (function (extra) {
                          return onChangeValue(setValue, extra);
                        })
                    })
              ]
            });
}

var InterfaceInput = {
  onChangeValue: onChangeValue,
  make: TableInput$InterfaceInput
};

function TableInput$InputOutputSelector(props) {
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx(TableInput$InterfaceInput, {
                      label: "Inputs",
                      value: props.inputs,
                      setValue: props.setInputs
                    }),
                JsxRuntime.jsx(TableInput$InterfaceInput, {
                      label: "Outputs",
                      value: props.outputs,
                      setValue: props.setOutputs
                    })
              ],
              className: "flex flex-row gap-5"
            });
}

var InputOutputSelector = {
  make: TableInput$InputOutputSelector
};

function TableInput$ValuePickerOption(props) {
  var value = props.value;
  return JsxRuntime.jsx("option", {
              children: Belnap.string_of_value(value),
              value: Belnap.string_of_value(value)
            });
}

var ValuePickerOption = {
  make: TableInput$ValuePickerOption
};

function onChangeValue$1(setValue, ev) {
  var valueString = ev.currentTarget.value;
  var value = Belnap.value_of_string(valueString);
  if (value !== undefined) {
    return setValue(value);
  }
  
}

function TableInput$ValuePicker(props) {
  var setValue = props.setValue;
  return JsxRuntime.jsxs("select", {
              children: [
                JsxRuntime.jsx(TableInput$ValuePickerOption, {
                      value: "Bottom"
                    }),
                JsxRuntime.jsx(TableInput$ValuePickerOption, {
                      value: "False"
                    }),
                JsxRuntime.jsx(TableInput$ValuePickerOption, {
                      value: "True"
                    }),
                JsxRuntime.jsx(TableInput$ValuePickerOption, {
                      value: "Top"
                    })
              ],
              className: "p-2 rounded-lg m-1",
              value: Belnap.string_of_value(props.value),
              onChange: (function (extra) {
                  return onChangeValue$1(setValue, extra);
                })
            });
}

var ValuePicker = {
  onChangeValue: onChangeValue$1,
  make: TableInput$ValuePicker
};

function updateRowPortion(rowPortion, index, value) {
  return rowPortion.map(function (v, i) {
              if (i === index) {
                return value;
              } else {
                return v;
              }
            });
}

function updateRow(row, setRow, isInputs, index, value) {
  var inputs = row[0];
  var match = isInputs ? [
      updateRowPortion(inputs, index, value),
      row[1]
    ] : [
      inputs,
      updateRowPortion(inputs, index, value)
    ];
  return setRow([
              match[0],
              match[1]
            ]);
}

function TableInput$TruthTableRow(props) {
  var row = props.row;
  var setRow = props.setRow;
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx("div", {
                      children: MoreReact.mapi(row[0], (function (input, key) {
                              return JsxRuntime.jsx(TableInput$ValuePicker, {
                                          value: input,
                                          setValue: (function (extra) {
                                              return updateRow(row, setRow, true, key, extra);
                                            })
                                        }, key.toString());
                            }))
                    }),
                JsxRuntime.jsx("div", {
                      children: MoreReact.mapi(row[1], (function (input, key) {
                              return JsxRuntime.jsx(TableInput$ValuePicker, {
                                          value: input,
                                          setValue: (function (extra) {
                                              return updateRow(row, setRow, false, key, extra);
                                            })
                                        }, key.toString());
                            }))
                    })
              ],
              className: "flex flex-row gap-5"
            });
}

var TruthTableRow = {
  updateRowPortion: updateRowPortion,
  updateRow: updateRow,
  make: TableInput$TruthTableRow
};

function initialiseRow(inputs, outputs) {
  var inputs$1 = Core__Array.fromInitializer(inputs, (function (param) {
          return "Bottom";
        }));
  var outputs$1 = Core__Array.fromInitializer(outputs, (function (param) {
          return "Bottom";
        }));
  return [
          inputs$1,
          outputs$1
        ];
}

function onClickAddRow(inputs, outputs, setRows, param) {
  return setRows(function (oldRows) {
              return Belt_Array.concatMany([
                          oldRows,
                          [initialiseRow(inputs, outputs)]
                        ]);
            });
}

function canComputeExpression(rows) {
  return Core__Array.reduce(rows, [
                [],
                true
              ], (function (param, param$1) {
                  var inputs = param$1[0];
                  var dict = param[0];
                  if (!param[1] || dict.includes(inputs)) {
                    return [
                            dict,
                            false
                          ];
                  } else {
                    return [
                            Belt_Array.concatMany([
                                  dict,
                                  [inputs]
                                ]),
                            true
                          ];
                  }
                }))[1];
}

function onClickComputeExpression(setExps, setFullTable, setFalsyTable, setTruthyTable, inputs, outputs, rows, param) {
  var match = Expression.expressions_of_table(rows, inputs, outputs);
  var expressions = match[2];
  var truthyTable = match[1];
  var falsyTable = match[0];
  setFullTable(function (param) {
        return rows;
      });
  setFalsyTable(function (param) {
        return falsyTable;
      });
  setTruthyTable(function (param) {
        return truthyTable;
      });
  return setExps(function (param) {
              return expressions;
            });
}

function updateRows(rows, setRows, index, newRow) {
  var newRows = rows.map(function (row, i) {
        if (i === index) {
          return newRow;
        } else {
          return row;
        }
      });
  return setRows(function (param) {
              return newRows;
            });
}

function TableInput$TruthTableGrid(props) {
  var outputs = props.outputs;
  var inputs = props.inputs;
  var match = React.useState(function () {
        return [];
      });
  var setRows = match[1];
  var rows = match[0];
  var match$1 = React.useState(function () {
        
      });
  var setExps = match$1[1];
  var exps = match$1[0];
  var match$2 = React.useState(function () {
        
      });
  var setTable = match$2[1];
  var table = match$2[0];
  var match$3 = React.useState(function () {
        
      });
  var setFalsyTable = match$3[1];
  var falsyTable = match$3[0];
  var match$4 = React.useState(function () {
        
      });
  var setTruthyTable = match$4[1];
  var truthyTable = match$4[0];
  React.useEffect((function () {
          setRows(function (param) {
                return [];
              });
        }), [
        inputs,
        outputs
      ]);
  return JsxRuntime.jsxs("div", {
              children: [
                rows.length === 0 ? "" : JsxRuntime.jsx("div", {
                        children: MoreReact.mapi(rows, (function (row, key) {
                                return JsxRuntime.jsx(TableInput$TruthTableRow, {
                                            row: row,
                                            setRow: (function (extra) {
                                                return updateRows(rows, setRows, key, extra);
                                              })
                                          }, key.toString());
                              }))
                      }),
                JsxRuntime.jsxs("div", {
                      children: [
                        JsxRuntime.jsx("button", {
                              children: "Add row",
                              className: "p-2 border rounded-lg",
                              onClick: (function (extra) {
                                  return onClickAddRow(inputs, outputs, setRows, extra);
                                })
                            }),
                        canComputeExpression(rows) ? JsxRuntime.jsx("button", {
                                children: "Compute expression",
                                className: "p-2 border rounded-lg",
                                onClick: (function (extra) {
                                    return onClickComputeExpression(setExps, setTable, setFalsyTable, setTruthyTable, inputs, outputs, rows, extra);
                                  })
                              }) : ""
                      ],
                      className: "flex flex-row gap-5"
                    }),
                table !== undefined && falsyTable !== undefined && truthyTable !== undefined ? JsxRuntime.jsxs("div", {
                        children: [
                          JsxRuntime.jsxs("div", {
                                children: [
                                  JsxRuntime.jsx("div", {
                                        children: "Original",
                                        className: "pb-2"
                                      }),
                                  JsxRuntime.jsx(TruthTable.FromTable.make, {
                                        table: table
                                      })
                                ]
                              }),
                          JsxRuntime.jsxs("div", {
                                children: [
                                  JsxRuntime.jsx("div", {
                                        children: "Falsy",
                                        className: "pb-2"
                                      }),
                                  JsxRuntime.jsx(TruthTable.FromTable.make, {
                                        table: falsyTable
                                      })
                                ]
                              }),
                          JsxRuntime.jsxs("div", {
                                children: [
                                  JsxRuntime.jsx("div", {
                                        children: "Truthy",
                                        className: "pb-2"
                                      }),
                                  JsxRuntime.jsx(TruthTable.FromTable.make, {
                                        table: truthyTable
                                      })
                                ]
                              })
                        ],
                        className: "flex flex-row gap-10"
                      }) : "",
                exps !== undefined ? MoreReact.mapi(exps, (function (exp, i) {
                          return JsxRuntime.jsx("div", {
                                      children: JsxRuntime.jsx(BetterReactMathjax.MathJaxContext, {
                                            children: JsxRuntime.jsx(Mathjax.make, {
                                                  children: Mathjax.inline(Expression.latex_of_expression(exp)),
                                                  inline: true,
                                                  dynamic: true
                                                })
                                          })
                                    }, i.toString());
                        })) : ""
              ],
              className: "flex flex-col items-start gap-4"
            });
}

var TruthTableGrid = {
  initialiseRow: initialiseRow,
  onClickAddRow: onClickAddRow,
  canComputeExpression: canComputeExpression,
  onClickComputeExpression: onClickComputeExpression,
  updateRows: updateRows,
  make: TableInput$TruthTableGrid
};

function TableInput(props) {
  var match = React.useState(function () {
        return 0;
      });
  var inputs = match[0];
  var match$1 = React.useState(function () {
        return 0;
      });
  var outputs = match$1[0];
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsx("div", {
                      children: "Define a truth table below to compute its logical expression."
                    }),
                JsxRuntime.jsx(TableInput$InputOutputSelector, {
                      inputs: inputs,
                      setInputs: match[1],
                      outputs: outputs,
                      setOutputs: match$1[1]
                    }),
                inputs !== undefined && outputs !== undefined ? JsxRuntime.jsx(TableInput$TruthTableGrid, {
                        inputs: inputs,
                        outputs: outputs
                      }) : ""
              ],
              className: "flex flex-col gap-4"
            });
}

var make = TableInput;

export {
  InterfaceInput ,
  InputOutputSelector ,
  ValuePickerOption ,
  ValuePicker ,
  TruthTableRow ,
  TruthTableGrid ,
  make ,
}
/* react Not a pure module */
