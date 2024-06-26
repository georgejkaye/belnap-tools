module ParjserInt = {
  type t
}

type parjser
type combinator

@val external exactlyInt: int => parjser = "exactly"
@val external exactlyString: string => parjser = "exactly"

@val external pipe: (parjser, combinator) => parjser = "pipe"
@val external between: (parjser, parjser) => combinator = "between"

module type Result = {
  type result
}

module type Parser = {
  type t
  type res
  type parseOptions
  let read: (~options: parseOptions=?) => t
}

module Combinator = (InParser: Parser, OutParser: Parser) => {
  type t
  type from = InParser.t
  type to = OutParser.t
  @val external map: (InParser.t, InParser.res => OutParser.res) => OutParser.t = "map"
  @val external pipe: (InParser.t, t) => OutParser.t = "pipe"
}

module Pipe = {}

module ParserMethods = (Parser: Parser) => {
  @get external isOkay: Parser.t => bool = "isOkay"
  @get external reason: Parser.t => string = "reason"
  @val external parse: (Parser.t, string) => Parser.res = "parse"
}

type intOptions = {
  allowSign: bool,
  base: int,
}

module IntParser: Parser = {
  type t
  type res = int
  type parseOptions = intOptions
  @val external read: (~options: parseOptions=?) => t = "int"
}

module Result = (Parser: Parser) => {
  type t
  type res
}
