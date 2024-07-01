module InterfaceInput = {
  let onChangeValue = (setValue, ev) => {
    let value = Int.fromString(JsxEvent.Form.target(ev)["value"])
    switch value {
    | None => setValue(_ => None)
    | Some(i) => setValue(_ => Some(i))
    }
  }
  @react.component
  let make = (~label, ~value, ~setValue) => {
    let id = String.toLowerCase(label)
    <>
      <label htmlFor={id} className={"py-2"}> {React.string(label)} </label>
      <input
        id
        className={"w-20 border p-2 rounded-lg"}
        type_={"number"}
        min={"0"}
        value={switch value {
        | None => ""
        | Some(i) => Int.toString(i)
        }}
        onChange={onChangeValue(setValue, ...)}
      />
    </>
  }
}

module InputOutputSelector = {
  @react.component
  let make = (~inputs, ~setInputs, ~outputs, ~setOutputs) => {
    <div className={"flex flex-row gap-5"}>
      <InterfaceInput label={"Inputs"} value={inputs} setValue={setInputs} />
      <InterfaceInput label={"Outputs"} value={outputs} setValue={setOutputs} />
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

module ValuePicker = {
  let onChangeValue = (setValue, ev) => {
    let valueString = ReactEvent.Form.currentTarget(ev)["value"]
    switch Belnap.value_of_string(valueString) {
    | None => ()
    | Some(value) => setValue(value)
    }
  }
  @react.component
  let make = (~value, ~setValue) => {
    <select
      className="p-2 rounded-lg m-1"
      value={Belnap.string_of_value(value)}
      onChange={onChangeValue(setValue, ...)}>
      <ValuePickerOption value={Belnap.Bottom} />
      <ValuePickerOption value={Belnap.False} />
      <ValuePickerOption value={Belnap.True} />
      <ValuePickerOption value={Belnap.Top} />
    </select>
  }
}

module TruthTableRow = {
  let updateRowPortion = (rowPortion, index, value) => {
    Array.mapWithIndex(rowPortion, (v, i) => i == index ? value : v)
  }
  let updateRow = (row, setRow, isInputs, index, value) => {
    let (inputs, outputs) = row
    let (newInputs, newOutputs) = isInputs
      ? (updateRowPortion(inputs, index, value), outputs)
      : (inputs, updateRowPortion(inputs, index, value))
    setRow((newInputs, newOutputs))
  }
  @react.component
  let make = (~row, ~setRow) => {
    let (inputs, outputs) = row
    <div className="flex flex-row gap-5">
      <div>
        {MoreReact.mapi(inputs, (input, key) =>
          <ValuePicker
            key={Int.toString(key)} value={input} setValue={updateRow(row, setRow, true, key, ...)}
          />
        )}
      </div>
      <div>
        {MoreReact.mapi(outputs, (input, key) =>
          <ValuePicker
            key={Int.toString(key)} value={input} setValue={updateRow(row, setRow, false, key, ...)}
          />
        )}
      </div>
    </div>
  }
}

module TruthTableGrid = {
  let initialiseRow = (inputs, outputs) => {
    let inputs = Array.fromInitializer(~length=inputs, _ => Belnap.Bottom)
    let outputs = Array.fromInitializer(~length=outputs, _ => Belnap.Bottom)
    (inputs, outputs)
  }
  let onClickAddRow = (inputs, outputs, setRows, _) => {
    setRows(oldRows => [...oldRows, initialiseRow(inputs, outputs)])
  }
  let canComputeExpression = rows => {
    Array.reduce(rows, ([], true), ((dict, res), (inputs, _)) =>
      !res || Array.includes(dict, inputs) ? (dict, false) : ([...dict, inputs], true)
    )->snd
  }
  let onClickComputeExpression = (
    setExps,
    setFullTable,
    setFalsyTable,
    setTruthyTable,
    inputs,
    outputs,
    rows,
    _,
  ) => {
    let (falsyTable, truthyTable, expressions) = Expression.expressions_of_table(
      rows,
      inputs,
      outputs,
    )
    setFullTable(_ => Some(rows))
    setFalsyTable(_ => Some(falsyTable))
    setTruthyTable(_ => Some(truthyTable))
    setExps(_ => Some(expressions))
  }
  let updateRows = (rows, setRows, index, newRow) => {
    let newRows = Array.mapWithIndex(rows, (row, i) => i == index ? newRow : row)
    setRows(_ => newRows)
  }
  @react.component
  let make = (~inputs, ~outputs) => {
    let (rows, setRows) = React.useState(_ => [])
    let (exps, setExps) = React.useState(_ => None)
    let (table, setTable) = React.useState(_ => None)
    let (falsyTable, setFalsyTable) = React.useState(_ => None)
    let (truthyTable, setTruthyTable) = React.useState(_ => None)
    React.useEffect(() => {
      setRows(_ => [])
      None
    }, [inputs, outputs])
    <div className="flex flex-col items-start gap-4 py-4">
      {Array.length(rows) == 0
        ? React.string("")
        : <div>
            {MoreReact.mapi(rows, (row, key) =>
              <TruthTableRow
                key={Int.toString(key)} row setRow={updateRows(rows, setRows, key, ...)}
              />
            )}
          </div>}
      <div className="flex flex-row gap-5">
        <button
          className="p-2 border rounded-lg" onClick={onClickAddRow(inputs, outputs, setRows, ...)}>
          {React.string("Add row")}
        </button>
        {!canComputeExpression(rows)
          ? React.string("")
          : <button
              className="p-2 border rounded-lg"
              onClick={onClickComputeExpression(
                setExps,
                setTable,
                setFalsyTable,
                setTruthyTable,
                inputs,
                outputs,
                rows,
                ...
              )}>
              {React.string("Compute expression")}
            </button>}
      </div>
      {switch (table, falsyTable, truthyTable) {
      | (Some(t), Some(ft), Some(tt)) =>
        <div>
          <div> {Table.string_of_table(t)->React.string} </div>
          <div> {Table.string_of_table(ft)->React.string} </div>
          <div> {Table.string_of_table(tt)->React.string} </div>
        </div>

      | _ => React.string("")
      }}
      {switch exps {
      | None => React.string("")
      | Some(exps) =>
        MoreReact.mapi(exps, (exp, i) =>
          <div key={Int.toString(i)}> {Expression.string_of_expression(exp)->React.string} </div>
        )
      }}
    </div>
  }
}

@react.component
let make = () => {
  let (inputs, setInputs) = React.useState(_ => Some(0))
  let (outputs, setOutputs) = React.useState(_ => Some(0))
  <div>
    <InputOutputSelector inputs setInputs outputs setOutputs />
    {switch (inputs, outputs) {
    | (Some(inputs), Some(outputs)) => <TruthTableGrid inputs outputs />
    | _ => React.string("")
    }}
  </div>
}
