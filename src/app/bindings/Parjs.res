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

module Combinators = {
  @module("parjs/combinators") @val
  external then: parser<'b> => combinator<'a, ('a, 'b)> = "then"
  @module("parjs/combinators") @val
  external map: ('a => 'b) => combinator<'a, 'b> = "map"
  @module("parjs/combinators") @val
  external or: parser<'b> => combinator<'b, 'b> = "or"
  @module("parjs/combinators") @val
  external betweenOne: parser<'b> => combinator<'a, 'a> = "between"
  @module("parjs/combinators") @val
  external betweenTwo: (parser<'b>, parser<'c>) => combinator<'a, 'a> = "between"
  @module("parjs/combinators") @val
  external maybe: unit => combinator<'a, 'a> = "maybe"
  @module("parjs/combinators") @val
  external later: unit => parser<'a> = "later"

  type recovery = {kind: kind}

  @module("parjs/combinators") @val
  external recover: (unit => recovery) => combinator<'a, 'a> = "recover"
}

@send external pipe: (parser<'a>, combinator<'a, 'b>) => parser<'b> = "pipe"
@send external init: (parser<'a>, parser<'a>) => unit = "init"
@send external debug: parser<'a> => parser<'a> = "debug"
@send external expects: (parser<'a>, string) => parser<'a> = "expects"

let then = (p1, p2) => pipe(p1, Combinators.then(p2))
let map = (p1, f) => pipe(p1, Combinators.map(f))
let or = (p1, p2) => pipe(p1, Combinators.or(p2))
let betweenOne = (p1, surrounding) => pipe(p1, Combinators.betweenOne(surrounding))
let betweenTwo = (p1, pre, post) => pipe(p1, Combinators.betweenTwo(pre, post))
let betweenStrings = (p1, pre, post) => betweenTwo(p1, string(pre), string(post))
let later = () => Combinators.later()
let maybe = p1 => pipe(p1, Combinators.maybe())
let recover = (p1, recovery) => pipe(p1, Combinators.recover(recovery))
let recoverSoft = p1 => recover(p1, () => {kind: Soft})

type result<'a>
@send external parse: (parser<'a>, string) => result<'a> = "parse"

@get external isOkay: result<'a> => bool = "isOkay"
@get external reason: result<'a> => string = "reason"
@get external kind: result<'a> => kind = "kind"
@get external value: result<'a> => 'a = "value"

@send external toString: result<'a> => string = "toString"
