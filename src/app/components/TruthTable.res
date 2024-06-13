module InputOutputSelector = {
  let onChangeValue = (setValue, ev) => {
    let value = Int.fromString(JsxEvent.Form.target(ev)["value"])
    Console.log(value)
    switch value {
    | None => setValue(_ => None)
    | Some(i) => setValue(_ => Some(i))
    }
  }
  @react.component
  let make = (~inputs, ~setInputs, ~outputs, ~setOutputs) => {
    <div>
      <input
        type_={"number"}
        value={switch inputs {
        | None => ""
        | Some(i) => Int.toString(i)
        }}
        onChange={onChangeValue(setInputs, ...)}
      />
      <input
        type_={"number"}
        value={switch outputs {
        | None => ""
        | Some(i) => Int.toString(i)
        }}
        onChange={onChangeValue(setOutputs, ...)}
      />
    </div>
  }
}

module ValuePickerOption = {
  @react.component
  let make = (~value) =>
    <option value={Belnap.string_of_value(value)}>
      {Belnap.string_of_value(value)->React.string}
    </option>
}

module TruthTableRow = {
  @react.component
  let make = (~row) => {
    let (inputs, outputs) = row
    <div>
      {MoreReact.mapi(inputs, (input, key) =>
        <select>
          <ValuePickerOption value={Belnap.Bottom} />
          <ValuePickerOption value={Belnap.False} />
          <ValuePickerOption value={Belnap.True} />
          <ValuePickerOption value={Belnap.Top} />
        </select>
      )}
    </div>
  }
}

module TruthTableGrid = {
  @react.component
  let make = (~inputs, ~outputs) => {
    let (rows, setRows) = React.useState(_ => [])
    <div> {MoreReact.mapi(rows, (row, key) => <TruthTableRow key={Int.toString(key)} row />)} </div>
  }
}

@react.component
let make = () => {
  let (inputs, setInputs) = React.useState(_ => Some(0))
  let (outputs, setOutputs) = React.useState(_ => Some(0))
  <div>
    <InputOutputSelector inputs setInputs outputs setOutputs />
    <TruthTableGrid inputs outputs />
  </div>
}
