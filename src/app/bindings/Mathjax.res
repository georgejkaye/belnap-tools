module Context = {
  @module("better-react-mathjax") @react.component
  external make: (~children: React.element) => React.element = "MathJaxContext"
}

module Content = {
  @module("better-react-mathjax") @react.component
  external make: (~children: React.element, ~inline: bool, ~dynamic: bool) => React.element =
    "MathJax"
}

let inline = str => `\\(${str}\\)`
let display = str => `\\[${str}\\]`

@react.component
let make = (~children, ~inline=false, ~dynamic=false) => {
  <Content inline dynamic> {children} </Content>
}
