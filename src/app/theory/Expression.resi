type value = Bottom | False | True | Top
let string_of_value: value => string
let enumerate_inputs: int => array<array<value>>
let string_of_value_array: array<value> => string
type rec expression =
  | Variable(int)
  | Constant(value)
  | And(expression, expression)
  | Or(expression, expression)
  | Not(expression)
let test_exp: expression
let string_of_expression: expression => string
