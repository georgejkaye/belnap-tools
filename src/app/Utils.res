let concatAsStrings = (ss, ~delim=", ", fn) =>
  Array.reduce(ss, "", (acc, cur) => {
    let curString = fn(cur)
    if acc === "" {
      curString
    } else {
      `${acc}${delim}${curString}`
    }
  })

let concatStrings = (ss, ~delim=", ") => concatAsStrings(ss, ~delim, s => s)
