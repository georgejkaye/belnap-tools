module type Parser = {
  type t
  type res
}

module Combinator = (InParser: Parser, OutParser: Parser) => {
  type t
  type from = InParser.t
  type to = OutParser.t
  @send external map: (InParser.t, InParser.res => OutParser.res) => OutParser.t = "map"
  @send external pipe: (InParser.t, t) => OutParser.t = "pipe"
}

module Pipe = {}

type kind =
  | OK
  | Soft
  | Hard
  | Fatal

module ParserMethods = (Parser: Parser) => {
  type result

  @send external parse: (Parser.t, string) => result = "parse"

  @get external isOkay: result => bool = "isOkay"
  @get external reason: result => string = "reason"
  @get external kind: result => kind = "kind"
  @get external value: result => Parser.res = "value"
}

type intOptions = {
  allowSign: bool,
  base: int,
}

module IntParser: Parser with type res = int = {
  type t
  type res = int
}
module IntMethods = ParserMethods(IntParser)

@module("parjs") @val external int: (~options: intOptions=?) => IntParser.t = "int"

module Result = (Parser: Parser) => {
  type t
  type res
}
