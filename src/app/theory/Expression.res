type rec expression =
  | Variable(int)
  | Constant(Belnap.value)
  | And(expression, expression)
  | Or(expression, expression)
  | Not(expression)

let test_exp = And(Or(Variable(2), Constant(False)), Variable(1))

let rec string_of_expression = exp =>
  switch exp {
  | Variable(i) => `v${Int.toString(i)}`
  | Constant(v) => Belnap.string_of_value(v)
  | And(e1, e2) => `(${string_of_expression(e1)} ∧ ${string_of_expression(e2)})`
  | Or(e1, e2) => `(${string_of_expression(e1)} ∨ ${string_of_expression(e2)})`
  | Not(e) => `¬${string_of_expression(e)}`
  }

let function_to_rows = (fn, m) => {
  let input_values = Belnap.enumerate_inputs(m)
  Array.reduce(input_values, [], (acc, vs) => {
    switch fn(vs) {
    | None => acc
    | Some(ws) => [...acc, (vs, ws)]
    }
  })
}

let string_of_row = ((inputs, outputs)) => {
  let string_of_cells = elements =>
    Utils.concatAsStrings(elements, ~delim=" ", Belnap.string_of_value)
  let inputString = string_of_cells(inputs)
  let outputString = string_of_cells(outputs)
  `${inputString} | ${outputString}`
}

let string_of_table = (fn, m, n) => {
  let rows = function_to_rows(fn, m)
  Utils.concatAsStrings(rows, ~delim="\n", string_of_row)
}

let strings_of_table = (fn, m, n) => {
  let rows = function_to_rows(fn, m)
  Array.map(rows, string_of_row)
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

let expression_of_function = None
