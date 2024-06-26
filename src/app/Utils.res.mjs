// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";

function concatAsStrings(ss, delimOpt, fn) {
  var delim = delimOpt !== undefined ? delimOpt : ", ";
  return Core__Array.reduce(ss, "", (function (acc, cur) {
                var curString = fn(cur);
                if (acc === "") {
                  return curString;
                } else {
                  return acc + delim + curString;
                }
              }));
}

function concatStrings(ss, delimOpt) {
  var delim = delimOpt !== undefined ? delimOpt : ", ";
  return concatAsStrings(ss, delim, (function (s) {
                return s;
              }));
}

export {
  concatAsStrings ,
  concatStrings ,
}
/* No side effect */
