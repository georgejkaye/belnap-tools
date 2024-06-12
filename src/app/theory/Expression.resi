type expression
let string_of_expression: expression => string
let simplify: expression => expression
let eval: (Js.Map.t<int, Belnap.value>, expression) => Belnap.value

let test_table: Table.table
let test_falsy_table: Table.table
let test_truthy_table: Table.table
let test_exp: expression
