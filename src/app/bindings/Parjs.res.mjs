// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Combinators from "parjs/combinators";

function then(p1, p2) {
  return p1.pipe(Combinators.then(p2));
}

function map(p1, f) {
  return p1.pipe(Combinators.map(f));
}

function or(p1, p2) {
  return p1.pipe(Combinators.or(p2));
}

export {
  then ,
  map ,
  or ,
}
/* parjs/combinators Not a pure module */
