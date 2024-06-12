type table = array<(array<Belnap.value>, array<Belnap.value>)>
let table_of_function: (array<Belnap.value> => option<array<Belnap.value>>, int) => table
let falsy_table_of_table: table => table
let truthy_table_of_table: table => table

let string_of_table: table => string
let strings_of_table: table => array<string>
let string_of_function_table: (array<Belnap.value> => option<array<Belnap.value>>, int) => string
let strings_of_function_table: (
  array<Belnap.value> => option<array<Belnap.value>>,
  int,
) => array<string>
