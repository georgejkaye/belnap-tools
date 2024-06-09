let concatStrings = (ss, ~delim=", ") =>
  Array.reduce(ss, "", (acc, cur) => {
    if acc === "" {
      cur
    } else {
      `${acc}${delim}${cur}`
    }
  })
