type expression
let string_of_expression: expression => string
let simplify: expression => expression
let eval: (Js.Map.t<int, Belnap.value>, expression) => Belnap.value

type table
let table_of_function: (array<Belnap.value> => option<array<Belnap.value>>, int) => table

let string_of_table: table => string
let strings_of_table: table => array<string>
let string_of_function_table: (array<Belnap.value> => option<array<Belnap.value>>, int) => string
let strings_of_function_table: (
  array<Belnap.value> => option<array<Belnap.value>>,
  int,
) => array<string>

let test_table: table
let test_falsy_table: table
let test_truthy_table: table
let test_exp: expression
