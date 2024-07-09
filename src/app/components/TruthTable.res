module Table = {
  @react.component
  let make = (~rows, ~getInput, ~getOutput) => {
    <div className="flex flex-row">
      <div className="border-r pr-4 flex flex-col gap-2">
        {MoreReact.mapfor(rows, i => {
          <div className="font-mono">
            {Belnap.string_of_value_array(getInput(i))->React.string}
          </div>
        })}
      </div>
      <div className="px-4 flex flex-col gap-2">
        {MoreReact.mapfor(rows, i => {
          <div className="font-mono">
            {Belnap.string_of_value_array(getOutput(i))->React.string}
          </div>
        })}
      </div>
    </div>
  }
}

module FromFunction = {
  @react.component
  let make = (~inputs, ~function) => {
    let inputSignals = Belnap.enumerate_inputs(inputs)
    let rows = Array.length(inputSignals)
    let getInput = i => Array.getUnsafe(inputSignals, i)
    let getOutput = i => function(getInput(i))
    <Table rows getInput getOutput />
  }
}

module FromTable = {
  @react.component
  let make = (~table) => {
    let rows = Array.length(table)
    let getInput = i => fst(Array.getUnsafe(table, i))

    let getOutput = i => snd(Array.getUnsafe(table, i))

    <Table rows getInput getOutput />
  }
}

module FromInputOutputs = {
  @react.component
  let make = (~inputs, ~outputs) => {
    let rows = Array.length(inputs)
    let getInput = i => Array.getUnsafe(inputs, i)
    let getOutput = i => Array.getUnsafe(outputs, i)
    <Table rows getInput getOutput />
  }
}
