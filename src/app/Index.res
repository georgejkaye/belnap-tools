module P = {
  @react.component
  let make = (~children) => <p className="mb-2"> children </p>
}

let default = () =>
  <div>
    <h1 className="text-3xl font-semibold"> {"What is this about?"->React.string} </h1>
    <P>
      {React.string(` This is a simple template for a Next
      project using ReScript & TailwindCSS.`)}
    </P>
    <h2 className="text-2xl font-semibold mt-5"> {React.string("Quick Start")} </h2>
    <div className="flex flex-col gap-5">
      <div>
        {MoreReact.map(Expression.strings_of_table(Expression.test_table), row =>
          <div className="font-mono"> {row->React.string} </div>
        )}
      </div>
      <div>
        {MoreReact.map(Expression.strings_of_table(Expression.test_falsy_table), row =>
          <div className="font-mono"> {row->React.string} </div>
        )}
      </div>
      <div>
        {MoreReact.map(Expression.strings_of_table(Expression.test_truthy_table), row =>
          <div className="font-mono"> {row->React.string} </div>
        )}
      </div>
      <div className="font-mono">
        {Expression.string_of_expression(Expression.test_exp)->React.string}
      </div>
    </div>
  </div>
