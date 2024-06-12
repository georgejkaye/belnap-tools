type table = array<(array<Belnap.value>, array<Belnap.value>)>

let table_of_function = (fn, m) => {
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

let truthy_table_of_table = explode_rows(Belnap.truthy_of_value, false, ...)
let falsy_table_of_table = explode_rows(Belnap.falsy_of_value, true, ...)

let string_of_row = ((inputs, outputs)) => {
  let string_of_cells = elements =>
    Utils.concatAsStrings(elements, ~delim=" ", Belnap.string_of_value)
  let inputString = string_of_cells(inputs)
  let outputString = string_of_cells(outputs)
  `${inputString} | ${outputString}`
}

let string_of_table = rows => Utils.concatAsStrings(rows, ~delim="\n", string_of_row)
let strings_of_table = rows => Array.map(rows, string_of_row)

let string_of_function_table = (fn, m) => {
  let rows = table_of_function(fn, m)
  string_of_table(rows)
}

let strings_of_function_table = (fn, m) => {
  let rows = table_of_function(fn, m)
  strings_of_table(rows)
}
