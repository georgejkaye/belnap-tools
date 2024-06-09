type value = Bottom | False | True | Top

let string_of_value = value =>
  switch value {
  | Bottom => "⊥"
  | False => "f"
  | True => "t"
  | Top => "⊤"
  }

let all_values = [Bottom, False, True, Top]

let rec enumerate_inputs = m =>
  switch m {
  | 0 => [[]]
  | n =>
    let subs = enumerate_inputs(n - 1)
    Array.reduce(all_values, [], (acc, v) => Array.concat(acc, Array.map(subs, vs => [v, ...vs])))
  }

let string_of_value_array = vs => Utils.concatAsStrings(vs, ~delim=" ", string_of_value)

type rec expression =
  | Variable(int)
  | Constant(value)
  | And(expression, expression)
  | Or(expression, expression)
  | Not(expression)

let test_exp = And(Or(Variable(2), Constant(False)), Variable(1))

let rec string_of_expression = exp =>
  switch exp {
  | Variable(i) => `v${Int.toString(i)}`
  | Constant(v) => string_of_value(v)
  | And(e1, e2) => `(${string_of_expression(e1)} ∧ ${string_of_expression(e2)})`
  | Or(e1, e2) => `(${string_of_expression(e1)} ∨ ${string_of_expression(e2)})`
  | Not(e) => `¬${string_of_expression(e)}`
  }

let expression_of_function = None
