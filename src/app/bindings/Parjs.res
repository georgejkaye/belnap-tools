type kind =
  | OK
  | Soft
  | Hard
  | Fatal

type intOptions = {
  allowSign?: bool,
  base?: int,
}

type parser<'a>
@module("parjs") @val external int: (~options: intOptions=?) => parser<int> = "int"
@module("parjs") @val external string: string => parser<string> = "string"

type combinator<'a, 'b>
@module("parjs/combinators") @val
external thenCombinator: parser<'b> => combinator<'a, ('a, 'b)> = "then"
@module("parjs/combinators") @val
external mapCombinator: ('a => 'b) => combinator<'a, 'b> = "map"
@module("parjs/combinators") @val
external orCombinator: parser<'b> => combinator<'b, 'b> = "or"

@send external pipe: (parser<'a>, combinator<'a, 'b>) => parser<'b> = "pipe"

let then = (p1, p2) => pipe(p1, thenCombinator(p2))
let map = (p1, f) => pipe(p1, mapCombinator(f))
let or = (p1, p2) => pipe(p1, orCombinator(p2))

type result<'a>
@send external parse: (parser<'a>, string) => result<'a> = "parse"

@get external isOkay: result<'a> => bool = "isOkay"
@get external reason: result<'a> => string = "reason"
@get external kind: result<'a> => kind = "kind"
@get external value: result<'a> => 'a = "value"
