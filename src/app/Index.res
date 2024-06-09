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
    <div>
      <div>
        {MoreReact.map(Expression.enumerate_inputs(3), vs =>
          <div> {Expression.string_of_value_array(vs)->React.string} </div>
        )}
      </div>
    </div>
  </div>
