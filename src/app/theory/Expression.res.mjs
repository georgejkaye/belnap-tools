// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Table from "./Table.res.mjs";
import * as Belnap from "./Belnap.res.mjs";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";

function binop_of_exp(exp) {
  switch (exp.TAG) {
    case "And" :
        return "AndOp";
    case "Or" :
        return "OrOp";
    case "Join" :
        return "JoinOp";
    default:
      return ;
  }
}

function string_of_expression(exp) {
  var string_of_expression$p = function (parent, exp) {
    var string;
    switch (exp.TAG) {
      case "Variable" :
          string = "v" + exp._0.toString();
          break;
      case "Constant" :
          string = Belnap.string_of_value(exp._0);
          break;
      case "And" :
          string = string_of_expression$p({
                TAG: "BinOp",
                _0: "AndOp"
              }, exp._0) + " ∧ " + string_of_expression$p({
                TAG: "BinOp",
                _0: "AndOp"
              }, exp._1);
          break;
      case "Or" :
          string = string_of_expression$p({
                TAG: "BinOp",
                _0: "OrOp"
              }, exp._0) + " ∨ " + string_of_expression$p({
                TAG: "BinOp",
                _0: "OrOp"
              }, exp._1);
          break;
      case "Join" :
          string = string_of_expression$p({
                TAG: "BinOp",
                _0: "JoinOp"
              }, exp._0) + " ⊔ " + string_of_expression$p({
                TAG: "BinOp",
                _0: "JoinOp"
              }, exp._1);
          break;
      case "Not" :
          string = "¬" + string_of_expression$p({
                TAG: "UnOp",
                _0: "NotOp"
              }, exp._0);
          break;
      
    }
    var match = binop_of_exp(exp);
    if (parent !== undefined) {
      if (parent.TAG === "BinOp") {
        if (match !== undefined && parent._0 !== match) {
          return "(" + string + ")";
        } else {
          return string;
        }
      } else {
        return " (" + string + ")";
      }
    } else {
      return string;
    }
  };
  return string_of_expression$p(undefined, exp);
}

function simplify(_exp) {
  while(true) {
    var exp = _exp;
    switch (exp.TAG) {
      case "Variable" :
          return {
                  TAG: "Variable",
                  _0: exp._0
                };
      case "Constant" :
          return {
                  TAG: "Constant",
                  _0: exp._0
                };
      case "And" :
          var a = exp._0;
          var exit = 0;
          var exit$1 = 0;
          var exit$2 = 0;
          var exit$3 = 0;
          if (a.TAG === "Constant") {
            if (a._0 === "False") {
              return {
                      TAG: "Constant",
                      _0: "False"
                    };
            }
            exit$3 = 5;
          } else {
            exit$3 = 5;
          }
          if (exit$3 === 5) {
            var match = exp._1;
            if (match.TAG === "Constant") {
              if (match._0 === "False") {
                return {
                        TAG: "Constant",
                        _0: "False"
                      };
              }
              exit$2 = 4;
            } else {
              exit$2 = 4;
            }
          }
          if (exit$2 === 4) {
            if (a.TAG === "Constant") {
              switch (a._0) {
                case "True" :
                    _exp = exp._1;
                    continue ;
                case "Bottom" :
                case "Top" :
                    exit$1 = 3;
                    break;
                
              }
            } else {
              exit$1 = 3;
            }
          }
          if (exit$1 === 3) {
            var match$1 = exp._1;
            if (match$1.TAG === "Constant") {
              switch (match$1._0) {
                case "True" :
                    _exp = a;
                    continue ;
                case "Bottom" :
                case "Top" :
                    exit = 2;
                    break;
                
              }
            }
            
          }
          if (exit === 2 && a.TAG === "Constant") {
            return {
                    TAG: "Constant",
                    _0: Belnap.and_fn(a._0, exp._1._0)
                  };
          }
          return {
                  TAG: "And",
                  _0: simplify(a),
                  _1: simplify(exp._1)
                };
      case "Or" :
          var a$1 = exp._0;
          var exit$4 = 0;
          var exit$5 = 0;
          var exit$6 = 0;
          var exit$7 = 0;
          if (a$1.TAG === "Constant") {
            if (a$1._0 === "True") {
              return {
                      TAG: "Constant",
                      _0: "True"
                    };
            }
            exit$7 = 5;
          } else {
            exit$7 = 5;
          }
          if (exit$7 === 5) {
            var match$2 = exp._1;
            if (match$2.TAG === "Constant") {
              if (match$2._0 === "True") {
                return {
                        TAG: "Constant",
                        _0: "True"
                      };
              }
              exit$6 = 4;
            } else {
              exit$6 = 4;
            }
          }
          if (exit$6 === 4) {
            if (a$1.TAG === "Constant") {
              switch (a$1._0) {
                case "False" :
                    _exp = exp._1;
                    continue ;
                case "Bottom" :
                case "Top" :
                    exit$5 = 3;
                    break;
                
              }
            } else {
              exit$5 = 3;
            }
          }
          if (exit$5 === 3) {
            var match$3 = exp._1;
            if (match$3.TAG === "Constant") {
              switch (match$3._0) {
                case "False" :
                    _exp = a$1;
                    continue ;
                case "Bottom" :
                case "Top" :
                    exit$4 = 2;
                    break;
                
              }
            }
            
          }
          if (exit$4 === 2 && a$1.TAG === "Constant") {
            return {
                    TAG: "Constant",
                    _0: Belnap.or_fn(a$1._0, exp._1._0)
                  };
          }
          return {
                  TAG: "Or",
                  _0: simplify(a$1),
                  _1: simplify(exp._1)
                };
      case "Join" :
          var a$2 = exp._0;
          var exit$8 = 0;
          if (a$2.TAG === "Constant") {
            var a$3 = a$2._0;
            var b = exp._1;
            if (b.TAG === "Constant") {
              return {
                      TAG: "Constant",
                      _0: Belnap.join_fn(a$3, b._0)
                    };
            }
            switch (a$3) {
              case "Bottom" :
                  _exp = exp._1;
                  continue ;
              case "False" :
              case "True" :
                  break;
              case "Top" :
                  exit$8 = 2;
                  break;
              
            }
          } else {
            var match$4 = exp._1;
            if (match$4.TAG === "Constant") {
              switch (match$4._0) {
                case "Bottom" :
                    _exp = a$2;
                    continue ;
                case "False" :
                case "True" :
                    exit$8 = 2;
                    break;
                case "Top" :
                    return {
                            TAG: "Constant",
                            _0: "Top"
                          };
                
              }
            } else {
              exit$8 = 2;
            }
          }
          if (exit$8 === 2 && a$2.TAG === "Constant") {
            return {
                    TAG: "Constant",
                    _0: "Top"
                  };
          }
          return {
                  TAG: "Join",
                  _0: simplify(a$2),
                  _1: simplify(exp._1)
                };
      case "Not" :
          var a$4 = exp._0;
          switch (a$4.TAG) {
            case "Constant" :
                return {
                        TAG: "Constant",
                        _0: Belnap.not_fn(a$4._0)
                      };
            case "And" :
                return {
                        TAG: "Or",
                        _0: simplify({
                              TAG: "Not",
                              _0: a$4._0
                            }),
                        _1: simplify({
                              TAG: "Not",
                              _0: a$4._1
                            })
                      };
            case "Or" :
                return {
                        TAG: "And",
                        _0: simplify({
                              TAG: "Not",
                              _0: a$4._0
                            }),
                        _1: simplify({
                              TAG: "Not",
                              _0: a$4._1
                            })
                      };
            case "Variable" :
            case "Join" :
                return {
                        TAG: "Not",
                        _0: simplify(a$4)
                      };
            case "Not" :
                _exp = a$4._0;
                continue ;
            
          }
      
    }
  };
}

function $$eval(vars, exp) {
  var eval$p = function (exp) {
    switch (exp.TAG) {
      case "Variable" :
          var v = vars.get(exp._0);
          if (v !== undefined) {
            return v;
          }
          throw {
                RE_EXN_ID: "Not_found",
                Error: new Error()
              };
      case "Constant" :
          return exp._0;
      case "And" :
          return Belnap.and_fn(eval$p(exp._0), eval$p(exp._1));
      case "Or" :
          return Belnap.or_fn(eval$p(exp._0), eval$p(exp._1));
      case "Join" :
          return Belnap.join_fn(eval$p(exp._0), eval$p(exp._1));
      case "Not" :
          return Belnap.not_fn(eval$p(exp._0));
      
    }
  };
  return eval$p(exp);
}

function substitute(subs, exp) {
  switch (exp.TAG) {
    case "Variable" :
        var i = exp._0;
        var sub = subs.get(i);
        if (sub !== undefined) {
          return sub;
        } else {
          return {
                  TAG: "Variable",
                  _0: i
                };
        }
    case "Constant" :
        return {
                TAG: "Constant",
                _0: exp._0
              };
    case "And" :
        return {
                TAG: "And",
                _0: substitute(subs, exp._0),
                _1: substitute(subs, exp._1)
              };
    case "Or" :
        return {
                TAG: "Or",
                _0: substitute(subs, exp._0),
                _1: substitute(subs, exp._1)
              };
    case "Join" :
        return {
                TAG: "Join",
                _0: substitute(subs, exp._0),
                _1: substitute(subs, exp._1)
              };
    case "Not" :
        return {
                TAG: "Not",
                _0: substitute(subs, exp._0)
              };
    
  }
}

function get_conj(col_unit, high_value, get_col_op, inputs) {
  return Core__Array.reduceWithIndex(inputs, {
              TAG: "Constant",
              _0: col_unit
            }, (function (acc, cur, i) {
                if (cur === high_value) {
                  return get_col_op(acc, {
                              TAG: "Variable",
                              _0: i
                            });
                } else {
                  return acc;
                }
              }));
}

function get_dnf(col_unit, col_high_value, get_col_op, row_unit, row_high_value, get_row_op, output_index, rows) {
  return Core__Array.reduce(rows, {
              TAG: "Constant",
              _0: row_unit
            }, (function (acc, param) {
                var output = param[1][output_index];
                if (output === undefined) {
                  return acc;
                }
                if (Caml_option.valFromOption(output) !== row_high_value) {
                  return acc;
                }
                var element = get_conj(col_unit, col_high_value, get_col_op, param[0]);
                return get_row_op(acc, element);
              }));
}

function get_truthy_dnf(extra, extra$1) {
  return get_dnf("True", "True", (function (exp, v) {
                return {
                        TAG: "And",
                        _0: exp,
                        _1: v
                      };
              }), "Bottom", "True", (function (exp, v) {
                return {
                        TAG: "Or",
                        _0: exp,
                        _1: v
                      };
              }), extra, extra$1);
}

function get_falsy_dnf(extra, extra$1) {
  return get_dnf("False", "False", (function (exp, v) {
                return {
                        TAG: "Or",
                        _0: exp,
                        _1: v
                      };
              }), "Bottom", "False", (function (exp, v) {
                return {
                        TAG: "And",
                        _0: exp,
                        _1: v
                      };
              }), extra, extra$1);
}

function get_subs(left_translator, right_translator, m) {
  return new Map(Core__Array.fromInitializer((m << 1), (function (i) {
                    if (i % 2 === 0) {
                      return [
                              i,
                              left_translator(i)
                            ];
                    } else {
                      return [
                              i,
                              right_translator(i)
                            ];
                    }
                  })));
}

function expressions_of_table(table, m, n) {
  var falsy_table = Table.falsy_table_of_table(table);
  var truthy_table = Table.truthy_table_of_table(table);
  var falsy_subs = get_subs((function (i) {
          return {
                  TAG: "And",
                  _0: {
                    TAG: "Constant",
                    _0: "Bottom"
                  },
                  _1: {
                    TAG: "Variable",
                    _0: i
                  }
                };
        }), (function (i) {
          return {
                  TAG: "Not",
                  _0: {
                    TAG: "Or",
                    _0: {
                      TAG: "Constant",
                      _0: "Bottom"
                    },
                    _1: {
                      TAG: "Variable",
                      _0: i - 1 | 0
                    }
                  }
                };
        }), m);
  var truthy_subs = get_subs((function (i) {
          return {
                  TAG: "Not",
                  _0: {
                    TAG: "And",
                    _0: {
                      TAG: "Constant",
                      _0: "Bottom"
                    },
                    _1: {
                      TAG: "Variable",
                      _0: i
                    }
                  }
                };
        }), (function (i) {
          return {
                  TAG: "Or",
                  _0: {
                    TAG: "Constant",
                    _0: "Bottom"
                  },
                  _1: {
                    TAG: "Variable",
                    _0: i - 1 | 0
                  }
                };
        }), m);
  var expressions = Core__Array.fromInitializer(n, (function (i) {
          var falsy_exp = substitute(falsy_subs, get_falsy_dnf(i, falsy_table));
          var truthy_exp = substitute(truthy_subs, get_truthy_dnf(i, truthy_table));
          return {
                  TAG: "Join",
                  _0: falsy_exp,
                  _1: truthy_exp
                };
        }));
  return [
          falsy_table,
          truthy_table,
          expressions
        ];
}

function expressions_of_function(fn, m, n) {
  var table = Table.table_of_function(fn, m);
  var match = expressions_of_table(table, m, n);
  return [
          table,
          match[0],
          match[1],
          match[2]
        ];
}

var match = expressions_of_function((function (vs) {
        return [Belnap.not_fn(vs[0])];
      }), 1, 1);

var test_exp = match[3][0];

var test_table = match[0];

var test_falsy_table = match[1];

var test_truthy_table = match[2];

export {
  string_of_expression ,
  simplify ,
  $$eval ,
  expressions_of_table ,
  expressions_of_function ,
  test_table ,
  test_falsy_table ,
  test_truthy_table ,
  test_exp ,
}
/* match Not a pure module */
