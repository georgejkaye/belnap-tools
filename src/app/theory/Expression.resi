type expression
let string_of_expression: expression => string
let simplify: expression => expression
let eval: (Js.Map.t<int, Belnap.value>, expression) => Belnap.value

let expressions_of_table: (Table.table, int, int) => (Table.table, Table.table, array<expression>)
let expressions_of_function: (
  array<Belnap.value> => option<array<Belnap.value>>,
  int,
  int,
) => (Table.table, Table.table, Table.table, array<expression>)

let test_table: Table.table
let test_falsy_table: Table.table
let test_truthy_table: Table.table
let test_exp: expression

type parsingResult = Succ(expression) | Fail(string)
let parseExpression: string => parsingResult
