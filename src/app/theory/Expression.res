type rec expression =
  | Variable(int)
  | Constant(Belnap.value)
  | And(expression, expression)
  | Or(expression, expression)
  | Join(expression, expression)
  | Not(expression)

type binop = AndOp | OrOp | JoinOp
type unop = NotOp
type op = BinOp(binop) | UnOp(unop)

let binop_of_exp = exp =>
  switch exp {
  | And(_, _) => Some(AndOp)
  | Or(_, _) => Some(OrOp)
  | Join(_, _) => Some(JoinOp)
  | _ => None
  }

let string_of_expression = exp => {
  let rec string_of_expression' = (parent, exp) => {
    let string = switch exp {
    | Variable(i) => `v${Int.toString(i)}`
    | Constant(v) => Belnap.string_of_value(v)
    | And(e1, e2) =>
      `${string_of_expression'(Some(BinOp(AndOp)), e1)} ∧ ${string_of_expression'(
          Some(BinOp(AndOp)),
          e2,
        )}`
    | Or(e1, e2) =>
      `${string_of_expression'(Some(BinOp(OrOp)), e1)} ∨ ${string_of_expression'(
          Some(BinOp(OrOp)),
          e2,
        )}`
    | Join(e1, e2) =>
      `${string_of_expression'(Some(BinOp(JoinOp)), e1)} ⊔ ${string_of_expression'(
          Some(BinOp(JoinOp)),
          e2,
        )}`
    | Not(e) => `¬${string_of_expression'(Some(UnOp(NotOp)), e)}`
    }
    switch (parent, binop_of_exp(exp)) {
    | (Some(BinOp(parent_op)), Some(exp_op)) =>
      if parent_op == exp_op {
        string
      } else {
        `(${string})`
      }
    | (Some(UnOp(NotOp)), Some(_)) => `(${string})`
    | _ => string
    }
  }
  string_of_expression'(None, exp)
}

let latex_of_expression = exp => {
  let rec latex_of_expression' = (parent, exp) => {
    let string = switch exp {
    | Variable(i) => `v_{${Int.toString(i)}}`
    | Constant(v) => Belnap.latex_of_value(v)
    | And(e1, e2) =>
      `${latex_of_expression'(Some(BinOp(AndOp)), e1)} \\wedge ${latex_of_expression'(
          Some(BinOp(AndOp)),
          e2,
        )}`
    | Or(e1, e2) =>
      `${latex_of_expression'(Some(BinOp(OrOp)), e1)} \\vee ${latex_of_expression'(
          Some(BinOp(OrOp)),
          e2,
        )}`
    | Join(e1, e2) =>
      `${latex_of_expression'(Some(BinOp(JoinOp)), e1)} \\sqcup ${latex_of_expression'(
          Some(BinOp(JoinOp)),
          e2,
        )}`
    | Not(e) => `\\neg ${latex_of_expression'(Some(UnOp(NotOp)), e)}`
    }
    switch (parent, binop_of_exp(exp)) {
    | (Some(BinOp(parent_op)), Some(exp_op)) =>
      if parent_op == exp_op {
        string
      } else {
        `(${string})`
      }
    | (Some(UnOp(NotOp)), Some(_)) => `(${string})`
    | _ => string
    }
  }
  latex_of_expression'(None, exp)
}

let rec functionOfExpression = exp => inputs => {
  switch exp {
  | Variable(i) => Array.getUnsafe(inputs, i)
  | Constant(v) => v
  | And(e1, e2) => Belnap.and_fn(functionOfExpression(e1)(inputs), functionOfExpression(e2)(inputs))
  | Or(e1, e2) => Belnap.or_fn(functionOfExpression(e1)(inputs), functionOfExpression(e2)(inputs))
  | Join(e1, e2) =>
    Belnap.join_fn(functionOfExpression(e1)(inputs), functionOfExpression(e2)(inputs))
  | Not(e1) => Belnap.not_fn(functionOfExpression(e1)(inputs))
  }
}

let highestVariable = exp => {
  let rec highestVariable' = (highest, exp) => {
    switch exp {
    | Variable(i) => Js.Math.max_int(i, highest)
    | Constant(_) => highest
    | And(e1, e2) => highestVariable'(highestVariable'(highest, e1), e2)
    | Or(e1, e2) => highestVariable'(highestVariable'(highest, e1), e2)
    | Join(e1, e2) => highestVariable'(highestVariable'(highest, e1), e2)
    | Not(e1) => highestVariable'(highest, e1)
    }
  }
  highestVariable'(-1, exp)
}

let rec simplify = exp =>
  switch exp {
  | Constant(v) => Constant(v)
  | Variable(i) => Variable(i)
  | And(Constant(False), _) => Constant(False)
  | And(_, Constant(False)) => Constant(False)
  | And(Constant(True), b) => simplify(b)
  | And(a, Constant(True)) => simplify(a)
  | And(Constant(a), Constant(b)) => Constant(Belnap.and_fn(a, b))
  | And(a, b) => And(simplify(a), simplify(b))
  | Or(Constant(True), _) => Constant(True)
  | Or(_, Constant(True)) => Constant(True)
  | Or(Constant(False), b) => simplify(b)
  | Or(a, Constant(False)) => simplify(a)
  | Or(Constant(a), Constant(b)) => Constant(Belnap.or_fn(a, b))
  | Or(a, b) => Or(simplify(a), simplify(b))
  | Not(Constant(a)) => Constant(Belnap.not_fn(a))
  | Not(Not(a)) => simplify(a)
  | Not(And(a, b)) => Or(simplify(Not(a)), simplify(Not(b)))
  | Not(Or(a, b)) => And(simplify(Not(a)), simplify(Not(b)))
  | Not(a) => Not(simplify(a))
  | Join(Constant(a), Constant(b)) => Constant(Belnap.join_fn(a, b))
  | Join(Constant(Bottom), b) => simplify(b)
  | Join(a, Constant(Bottom)) => simplify(a)
  | Join(Constant(Top), _) => Constant(Top)
  | Join(_, Constant(Top)) => Constant(Top)
  | Join(a, b) => Join(simplify(a), simplify(b))
  }

let eval = (vars, exp) => {
  let rec eval' = exp =>
    switch exp {
    | Constant(v) => v
    | Variable(i) =>
      switch Map.get(vars, i) {
      | None => raise(Not_found)
      | Some(v) => v
      }
    | And(a, b) => Belnap.and_fn(eval'(a), eval'(b))
    | Or(a, b) => Belnap.or_fn(eval'(a), eval'(b))
    | Not(a) => Belnap.not_fn(eval'(a))
    | Join(a, b) => Belnap.join_fn(eval'(a), eval'(b))
    }
  eval'(exp)
}

let rec substitute = (subs, exp) =>
  switch exp {
  | Variable(i) =>
    switch Map.get(subs, i) {
    | None => Variable(i)
    | Some(sub) => sub
    }
  | Constant(v) => Constant(v)
  | And(e1, e2) => And(substitute(subs, e1), substitute(subs, e2))
  | Or(e1, e2) => Or(substitute(subs, e1), substitute(subs, e2))
  | Join(e1, e2) => Join(substitute(subs, e1), substitute(subs, e2))
  | Not(e) => Not(substitute(subs, e))
  }

let get_conj = (col_unit, high_value, get_col_op, inputs) =>
  Array.reduceWithIndex(inputs, Constant(col_unit), (acc, cur, i) =>
    if cur === high_value {
      get_col_op(acc, Variable(i))
    } else {
      acc
    }
  )

let get_dnf = (
  col_unit,
  col_high_value,
  get_col_op,
  row_unit,
  row_high_value,
  get_row_op,
  output_index,
  rows,
) => {
  Array.reduce(rows, Constant(row_unit), (acc, (inputs, outputs)) => {
    let output = outputs[output_index]
    switch output {
    | None => acc
    | Some(w) =>
      if w === row_high_value {
        let element = get_conj(col_unit, col_high_value, get_col_op, inputs)
        get_row_op(acc, element)
      } else {
        acc
      }
    }
  })
}

let get_truthy_dnf =
  get_dnf(
    Belnap.True,
    Belnap.True,
    (exp, v) => And(exp, v),
    Belnap.Bottom,
    Belnap.True,
    (exp, v) => Or(exp, v),
    ...
  )

let get_falsy_dnf =
  get_dnf(
    Belnap.False,
    Belnap.False,
    (exp, v) => Or(exp, v),
    Belnap.Bottom,
    Belnap.False,
    (exp, v) => And(exp, v),
    ...
  )

let get_subs = (left_translator, right_translator, m) =>
  Map.fromArray(
    Array.fromInitializer(~length=m * 2, i => {
      if mod(i, 2) == 0 {
        (i, left_translator(i))
      } else {
        (i, right_translator(i))
      }
    }),
  )

let expressions_of_table = (table, m, n) => {
  let falsy_table = Table.falsy_table_of_table(table)
  let truthy_table = Table.truthy_table_of_table(table)
  let falsy_subs = get_subs(
    i => And(Constant(Bottom), Variable(i)),
    i => Not(Or(Constant(Bottom), Variable(i - 1))),
    m,
  )
  let truthy_subs = get_subs(
    i => Not(And(Constant(Bottom), Variable(i))),
    i => Or(Constant(Bottom), Variable(i - 1)),
    m,
  )
  let expressions = Array.fromInitializer(~length=n, i => {
    let falsy_exp = substitute(falsy_subs, get_falsy_dnf(i, falsy_table))
    let truthy_exp = substitute(truthy_subs, get_truthy_dnf(i, truthy_table))
    Join(falsy_exp, truthy_exp)
  })
  (falsy_table, truthy_table, expressions)
}

let expressions_of_function = (fn, m, n) => {
  let table = Table.table_of_function(fn, m)
  let (falsy_table, truthy_table, expression) = expressions_of_table(table, m, n)
  (table, falsy_table, truthy_table, expression)
}

let constantParser = {
  let bottomParser = Parjs.string("n")->Parjs.map(_ => Belnap.Bottom)
  let trueParser = Parjs.string("t")->Parjs.map(_ => Belnap.True)
  let falseParser = Parjs.string("f")->Parjs.map(_ => Belnap.False)
  let topParser = Parjs.string("b")->Parjs.map(_ => Belnap.Top)
  bottomParser
  ->Parjs.or(trueParser)
  ->Parjs.or(falseParser)
  ->Parjs.or(topParser)
  ->Parjs.map(v => Constant(v))
}

let variableParser = {
  let identifierParser = Parjs.string("v")
  let numberParser = Parjs.int(~options={allowSign: false})
  identifierParser
  ->Parjs.then(numberParser)
  ->Parjs.map(((_, i)) => Variable(i))
}

let expressionParser = Parjs.later()
let bracketedParser = Parjs.later()

let notParser = Parjs.string("¬")->Parjs.map(_ => NotOp)

let valueParser = constantParser->Parjs.or(variableParser)->Parjs.expects("A value")

let unopArgumentParser = valueParser->Parjs.or(bracketedParser)

let unopParser =
  notParser
  ->Parjs.then(unopArgumentParser)
  ->Parjs.map(((_, exp)) => Not(exp))
  ->Parjs.expects("An unary expression")

let andParser = Parjs.string("&&")->Parjs.map(_ => AndOp)
let orParser = Parjs.string("||")->Parjs.map(_ => OrOp)
let joinParser = Parjs.string("VV")->Parjs.map(_ => JoinOp)

let spaceParser = Parjs.string(" ")->Parjs.expects("A space")
let maybeSpaceParser = spaceParser->Parjs.maybe

let binopLhsParser = bracketedParser->Parjs.or(valueParser)->Parjs.or(unopParser)

let binopParser = {
  let opParser = andParser->Parjs.or(orParser)->Parjs.or(joinParser)
  binopLhsParser
  ->Parjs.then(maybeSpaceParser)
  ->Parjs.then(opParser)
  ->Parjs.then(maybeSpaceParser)
  ->Parjs.then(expressionParser)
  ->Parjs.map((((((e1, _), op), _), e2)) =>
    switch op {
    | AndOp => And(e1, e2)
    | OrOp => Or(e1, e2)
    | JoinOp => Join(e1, e2)
    }
  )
  ->Parjs.expects("A binary operation")
}

let bracketedParser' =
  expressionParser->Parjs.betweenStrings("(", ")")->Parjs.expects("A bracketed expression")
Parjs.init(bracketedParser, bracketedParser')

let expressionParser' = {
  binopParser
  ->Parjs.recoverSoft
  ->Parjs.or(unopParser)
  ->Parjs.recoverSoft
  ->Parjs.or(valueParser)
  ->Parjs.recoverSoft
  ->Parjs.or(bracketedParser)
  ->Parjs.expects("An expression")
}

Parjs.init(expressionParser, expressionParser')

type parsingResult = Succ(expression) | Fail(string)

let parseExpression = str => {
  let res = Parjs.parse(expressionParser, str)
  switch Parjs.kind(res) {
  | OK => Succ(Parjs.value(res))
  | _ =>
    Console.log(Parjs.toString(res))
    Fail(Parjs.reason(res))
  }
}

let (
  test_table,
  test_falsy_table,
  test_truthy_table,
  test_exps,
) = expressions_of_function(vs => Some([Belnap.not_fn(Array.getUnsafe(vs, 0))]), 1, 1)
let test_exp = Array.getUnsafe(test_exps, 0)
