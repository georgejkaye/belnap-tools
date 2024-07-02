module Context = {
  @module("better-react-mathjax") @react.component
  external make: (~children: React.element) => React.element = "MathJaxContext"
}

module Content = {
  @module("better-react-mathjax") @react.component
  external make: (~children: React.element) => React.element = "MathJax"
}

let inline = str => `\\(${str}\\)`
let display = str => `\\[${str}\\]`

@react.component
let make = (~children: React.element) => {
  <Content> {children} </Content>
}
