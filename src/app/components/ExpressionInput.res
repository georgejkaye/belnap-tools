module InputBox = {
  @react.component
  let make = (~expression, ~setExpression) => {
    <div className="flex flex-row items-center gap-4">
      <input
        type_={"text"} className="w-34 rounded-lg border p-2" placeholder={"Type expression"}
      />
    </div>
  }
}

module CheatSheetItem = {
  @react.component
  let make = (~input, ~meaning) => {
    <div className="flex flex-row my-2 gap-2 mx-6">
      <div className="rounded-lg px-2 bg-gray-100 font-mono"> {React.string(input)} </div>
      <div className=""> {React.string(meaning)} </div>
    </div>
  }
}

module CheatSheet = {
  @react.component
  let make = () => {
    <div className="flex flex-row flex-wrap">
      <CheatSheetItem input={"n"} meaning={"⊥ value ('none')"} />
      <CheatSheetItem input={"b"} meaning={"f value ('false')"} />
      <CheatSheetItem input={"b"} meaning={"t value ('true')"} />
      <CheatSheetItem input={"b"} meaning={"⊤ value ('both')"} />
      <CheatSheetItem input={"<exp> && <exp>"} meaning={"AND"} />
      <CheatSheetItem input={"<exp> || <exp>"} meaning={"OR"} />
      <CheatSheetItem input={"<exp> VV <exp>"} meaning={"join"} />
      <CheatSheetItem input={"¬<exp>"} meaning={"NOT"} />
    </div>
  }
}

@react.component
let make = () => {
  let (expression, setExpression) = React.useState(_ => None)
  <div>
    <InputBox expression setExpression />
    <CheatSheet />
  </div>
}
