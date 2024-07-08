module InputBox = {
  @react.component
  let make = (~expression, ~setExpression) => {
    let (expressionText, setExpressionText) = React.useState(_ => "")
    let onChangeExpressionInput = e => {
      setExpressionText(JsxEvent.Form.target(e)["value"])
    }
    let parseExpression = e => {
      let expression = Expression.parseExpression(expressionText)
      switch expression {
      | Succ(exp) => setExpression(_ => Some(exp))
      | Fail(msg) => ()
      }
    }
    let onKeyDownExpressionInput = e => {
      if JsxEvent.Keyboard.key(e) == "Enter" {
        parseExpression(e)
      }
    }
    let onClickParseButton = e => {
      parseExpression(e)
    }
    <div className="flex flex-row items-center gap-2">
      <input
        type_={"text"}
        className="w-full rounded-lg border p-2"
        placeholder={"Type expression"}
        value={expressionText}
        onChange={onChangeExpressionInput}
        onKeyDown={onKeyDownExpressionInput}
      />
      <button className="rounded-lg border p-2" onClick={onClickParseButton}>
        {React.string("Parse")}
      </button>
    </div>
  }
}

module CheatSheetItem = {
  @react.component
  let make = (~input, ~meaning) => {
    <div className="flex flex-row my-2 gap-2">
      <div className="rounded-lg px-2 bg-gray-100 font-mono"> {React.string(input)} </div>
      <div className=""> {React.string(meaning)} </div>
    </div>
  }
}

module CheatSheet = {
  @react.component
  let make = () => {
    let (isOpen, setIsOpen) = React.useState(_ => false)
    let onClickCheatSheetButton = _ => setIsOpen(old => !old)
    <div className="flex flex-col border rounded-lg items-left p-4 gap-4">
      <button className="w-full text-left font-bold" onClick={onClickCheatSheetButton}>
        {React.string(`${isOpen ? "Hide" : "Show"} cheat sheet`)}
      </button>
      {!isOpen
        ? React.string("")
        : <div className="flex flex-row gap-5">
            <div className="flex flex-col">
              <CheatSheetItem input={"n"} meaning={"⊥ value ('none')"} />
              <CheatSheetItem input={"b"} meaning={"f value ('false')"} />
              <CheatSheetItem input={"b"} meaning={"t value ('true')"} />
              <CheatSheetItem input={"b"} meaning={"⊤ value ('both')"} />
            </div>
            <div className="flex flex-col">
              <CheatSheetItem input={"v0"} meaning={"variable 0"} />
              <CheatSheetItem input={"v1"} meaning={"variable 1"} />
              <CheatSheetItem input={"v2"} meaning={"variable 2"} />
              <div className="self-center"> {React.string("...")} </div>
            </div>
            <div className="flex flex-col">
              <CheatSheetItem input={"<exp> && <exp>"} meaning={"AND"} />
              <CheatSheetItem input={"<exp> || <exp>"} meaning={"OR"} />
              <CheatSheetItem input={"<exp> VV <exp>"} meaning={"join"} />
              <CheatSheetItem input={"¬<exp>"} meaning={"NOT"} />
            </div>
          </div>}
    </div>
  }
}

module ExpressionDisplay = {
  @react.component
  let make = (~expression) => {
    <div className="p-4 bg-green-800 text-yellow-300 rounded-lg">
      <Mathjax inline={true} dynamic={true}>
        {Expression.latex_of_expression(expression)->Mathjax.inline->React.string}
      </Mathjax>
    </div>
  }
}

module TableDisplay = {
  @react.component
  let make = (~expression) => {
    let numberOfInputs = Expression.highestVariable(expression) + 1
    let inputSignals = Belnap.enumerate_inputs(numberOfInputs)
    let expressionFunction = Expression.functionOfExpression(expression)
    <div className="flex flex-row">
      <div className="border-r pr-4 flex flex-col gap-2">
        {MoreReact.map(inputSignals, signal => {
          <div className="font-mono"> {Belnap.string_of_value_array(signal)->React.string} </div>
        })}
      </div>
      <div className="px-4 flex flex-col gap-2">
        {MoreReact.map(inputSignals, signal => {
          <div className="font-mono">
            {Belnap.string_of_value(expressionFunction(signal))->React.string}
          </div>
        })}
      </div>
    </div>
  }
}

@react.component
let make = () => {
  let (expression, setExpression) = React.useState(_ => None)
  <div className="flex flex-col gap-4">
    <InputBox expression setExpression />
    <CheatSheet />
    {switch expression {
    | None => React.string("")
    | Some(expression) =>
      <div className="flex flex-col gap-4">
        <ExpressionDisplay expression />
        <TableDisplay expression />
      </div>
    }}
  </div>
}
