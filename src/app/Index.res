type specificationType = TruthTable | Expression

module InputSelector = {
  @react.component
  let make = (~specType, ~setSpecType) => {
    let buttonStyle = "py-2 px-2 border rounded-lg"
    let inactiveButtonStyle = ""
    let activeButtonStyle = "bg-accent text-white font-bold"
    let (expressionButtonStyle, truthTableButtonStyle) = switch specType {
    | TruthTable => (inactiveButtonStyle, activeButtonStyle)
    | Expression => (activeButtonStyle, inactiveButtonStyle)
    }
    let onClickExpressionButton = _ => setSpecType(_ => Expression)
    let onClickTruthTableButton = _ => setSpecType(_ => TruthTable)
    <div className="flex flex-col gap-4">
      <div className="flex flex-row gap-4 align-items-center">
        <div className="font-bold py-2"> {React.string("Input method")} </div>
        <button
          className={`${buttonStyle} ${expressionButtonStyle}`} onClick={onClickExpressionButton}>
          {React.string("Expression")}
        </button>
        <button
          className={`${buttonStyle} ${truthTableButtonStyle}`} onClick={onClickTruthTableButton}>
          {React.string("Truth table")}
        </button>
      </div>
      <hr className="h-px border-1" />
    </div>
  }
}

let default = () => {
  let (specType, setSpecType) = React.useState(_ => Expression)
  <div className="flex flex-col gap-4">
    <InputSelector specType setSpecType />
    {switch specType {
    | TruthTable => <TruthTable />
    | Expression => <ExpressionInput />
    }}
  </div>
}
