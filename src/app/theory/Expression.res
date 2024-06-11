type rec expression =
  | Variable(int)
  | Constant(Belnap.value)
  | And(expression, expression)
  | Or(expression, expression)
  | Join(expression, expression)
  | Not(expression)

let test_exp = And(Or(Variable(2), Constant(False)), Variable(1))

let rec string_of_expression = exp =>
  switch exp {
  | Variable(i) => `v${Int.toString(i)}`
  | Constant(v) => Belnap.string_of_value(v)
  | And(e1, e2) => `(${string_of_expression(e1)} ∧ ${string_of_expression(e2)})`
  | Or(e1, e2) => `(${string_of_expression(e1)} ∨ ${string_of_expression(e2)})`
  | Join(e1, e2) => `(${string_of_expression(e1)} ⊔ ${string_of_expression(e2)})`
  | Not(e) => `¬${string_of_expression(e)}`
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

let rows_of_function = (fn, m) => {
  let input_values = Belnap.enumerate_inputs(m)
  Array.reduce(input_values, [], (acc, vs) => {
    switch fn(vs) {
    | None => acc
    | Some(ws) => [...acc, (vs, ws)]
    }
  })
}

let explode_row = (exploder, left_bit, (inputs, outputs)) => {
  let exploded_inputs = Array.reduce(inputs, [], (acc, cur) => {
    let (left, right) = exploder(cur)
    [...acc, left, right]
  })
  let exploded_outputs = Array.reduce(outputs, [], (acc, cur) => {
    let (left, right) = exploder(cur)
    let chosen = if left_bit {
      left
    } else {
      right
    }
    [...acc, chosen]
  })
  (exploded_inputs, exploded_outputs)
}

let explode_rows = (exploder, left, rows) => {
  Array.reduce(rows, [], (acc, row) => {
    [...acc, explode_row(exploder, left, row)]
  })
}

let truthy_explode_rows = explode_rows(Belnap.truthy_of_value, false, ...)
let falsy_explode_rows = explode_rows(Belnap.falsy_of_value, true, ...)

let string_of_row = ((inputs, outputs)) => {
  let string_of_cells = elements =>
    Utils.concatAsStrings(elements, ~delim=" ", Belnap.string_of_value)
  let inputString = string_of_cells(inputs)
  let outputString = string_of_cells(outputs)
  `${inputString} | ${outputString}`
}

let string_of_table = rows => Utils.concatAsStrings(rows, ~delim="\n", string_of_row)
let strings_of_table = rows => Array.map(rows, string_of_row)

let string_of_function_table = (fn, m, n) => {
  let rows = rows_of_function(fn, m)
  string_of_table(rows)
}

let strings_of_function_table = (fn, m, n) => {
  let rows = rows_of_function(fn, m)
  strings_of_table(rows)
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

let expressions_of_function = (fn, m, n) => {
  let table = rows_of_function(fn, m)
  let falsy_table = falsy_explode_rows(table)
  let truthy_table = truthy_explode_rows(table)
  let falsy_subs = get_subs(
    i => And(Constant(Bottom), Variable(i)),
    i => Not(Or(Constant(Bottom), Variable(i))),
    m,
  )
  let truthy_subs = get_subs(
    i => Not(And(Constant(Bottom), Variable(i))),
    i => Or(Constant(Bottom), Variable(i)),
    m,
  )
  let expressions = Array.fromInitializer(~length=n, i => {
    let falsy_exp = substitute(falsy_subs, get_falsy_dnf(i, falsy_table))
    let truthy_exp = substitute(truthy_subs, get_truthy_dnf(i, truthy_table))
    Join(falsy_exp, truthy_exp)
  })
  (table, falsy_table, truthy_table, expressions)
}

let (
  test_table,
  test_falsy_table,
  test_truthy_table,
  test_exps,
) = expressions_of_function(vs => Some([Belnap.not_fn(Array.getUnsafe(vs, 0))]), 1, 1)
let test_exp = Array.getUnsafe(test_exps, 0)
