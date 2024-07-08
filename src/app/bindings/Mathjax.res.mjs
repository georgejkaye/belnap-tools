// Generated by ReScript, PLEASE EDIT WITH CARE

import * as JsxRuntime from "react/jsx-runtime";
import * as BetterReactMathjax from "better-react-mathjax";

var Context = {};

var Content = {};

function inline(str) {
  return "\\(" + str + "\\)";
}

function display(str) {
  return "\\[" + str + "\\]";
}

function Mathjax(props) {
  return JsxRuntime.jsx(BetterReactMathjax.MathJax, {
              children: props.children
            });
}

var make = Mathjax;

export {
  Context ,
  Content ,
  inline ,
  display ,
  make ,
}
/* react/jsx-runtime Not a pure module */