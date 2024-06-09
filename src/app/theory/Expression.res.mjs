// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../Utils.res.mjs";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";

function string_of_value(value) {
  switch (value) {
    case "Bottom" :
        return "⊥";
    case "False" :
        return "f";
    case "True" :
        return "t";
    case "Top" :
        return "⊤";
    
  }
}

var all_values = [
  "Bottom",
  "False",
  "True",
  "Top"
];

function enumerate_inputs(m) {
  if (m === 0) {
    return [[]];
  }
  var subs = enumerate_inputs(m - 1 | 0);
  return Core__Array.reduce(all_values, [], (function (acc, v) {
                return acc.concat(subs.map(function (vs) {
                                return Belt_Array.concatMany([
                                            [v],
                                            vs
                                          ]);
                              }));
              }));
}

function string_of_value_array(vs) {
  return Utils.concatAsStrings(vs, " ", string_of_value);
}

function string_of_expression(exp) {
  switch (exp.TAG) {
    case "Variable" :
        return "v" + exp._0.toString();
    case "Constant" :
        return string_of_value(exp._0);
    case "And" :
        return "(" + string_of_expression(exp._0) + " ∧ " + string_of_expression(exp._1) + ")";
    case "Or" :
        return "(" + string_of_expression(exp._0) + " ∨ " + string_of_expression(exp._1) + ")";
    case "Not" :
        return "¬" + string_of_expression(exp._0);
    
  }
}

var test_exp = {
  TAG: "And",
  _0: {
    TAG: "Or",
    _0: {
      TAG: "Variable",
      _0: 2
    },
    _1: {
      TAG: "Constant",
      _0: "False"
    }
  },
  _1: {
    TAG: "Variable",
    _0: 1
  }
};

export {
  string_of_value ,
  enumerate_inputs ,
  string_of_value_array ,
  test_exp ,
  string_of_expression ,
}
/* No side effect */
