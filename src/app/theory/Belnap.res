type value = Bottom | False | True | Top

let string_of_value = value =>
  switch value {
  | Bottom => "⊥"
  | False => "f"
  | True => "t"
  | Top => "⊤"
  }

let value_of_string = string =>
  switch string {
  | "⊥" => Some(Bottom)
  | "f" => Some(False)
  | "t" => Some(True)
  | "⊤" => Some(Top)
  | _ => None
  }

let all_values = [Bottom, False, True, Top]

let truthy_of_value = value =>
  switch value {
  | Bottom => (Bottom, Bottom)
  | False => (True, Bottom)
  | True => (Bottom, True)
  | Top => (True, True)
  }

let falsy_of_value = value =>
  switch value {
  | Bottom => (Bottom, Bottom)
  | False => (False, Bottom)
  | True => (Bottom, False)
  | Top => (False, False)
  }

let rec enumerate_inputs = m =>
  switch m {
  | 0 => [[]]
  | n =>
    let subs = enumerate_inputs(n - 1)
    Array.reduce(all_values, [], (acc, v) => Array.concat(acc, Array.map(subs, vs => [v, ...vs])))
  }

let string_of_value_array = vs => Utils.concatAsStrings(vs, ~delim=" ", string_of_value)

let and_fn = (a, b) =>
  switch (a, b) {
  | (False, _) => False
  | (_, False) => False
  | (True, b) => b
  | (b, True) => b
  | (Bottom, Bottom) => Bottom
  | (Bottom, Top) => False
  | (Top, Bottom) => False
  | (Top, Top) => Top
  }

let or_fn = (a, b) =>
  switch (a, b) {
  | (True, _) => True
  | (_, True) => True
  | (False, b) => b
  | (b, False) => b
  | (Bottom, Bottom) => Bottom
  | (Bottom, Top) => True
  | (Top, Bottom) => True
  | (Top, Top) => Top
  }

let not_fn = a =>
  switch a {
  | Bottom => Bottom
  | True => False
  | False => True
  | Top => Top
  }

let join_fn = (a, b) =>
  switch (a, b) {
  | (Bottom, b) => b
  | (a, Bottom) => a
  | (Top, _) => Top
  | (_, Top) => Top
  | (True, True) => True
  | (True, False) => Top
  | (False, False) => False
  | (False, True) => Top
  }

let test_fn = vs => {
  let x = vs[0]
  let y = vs[1]
  switch (x, y) {
  | (Some(x), Some(y)) => Some([not_fn(and_fn(x, y))])
  | (_, _) => raise(Not_found)
  }
}
