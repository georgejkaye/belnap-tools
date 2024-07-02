module Link = Next.Link

module TopBar = {
  @react.component
  let make = () =>
    <nav
      className="w-full p-2 h-12 flex border-b border-gray-200 justify-between items-center text-sm bg-accent">
      <div className="text-white text-lg font-bold"> {React.string("Belnap tools")} </div>
    </nav>
}

@react.component
let make = (~children) => {
  let minWidth = ReactDOM.Style.make(~minWidth="20rem", ())
  <div style=minWidth className="flex flex-col items-center">
    <TopBar />
    <div className="max-w-5xl w-full lg:w-3/4 text-gray-900 font-base align-center">
      <main className="mt-4 mx-4">
        <Mathjax.Context> {children} </Mathjax.Context>
      </main>
    </div>
  </div>
}
