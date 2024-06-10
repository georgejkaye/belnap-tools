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
        {MoreReact.mapi(Expression.strings_of_table(Belnap.test_fn, 2, 1), (str, i) =>
          <div className="font-mono"> {str->React.string} </div>
        )}
      </div>
    </div>
  </div>
