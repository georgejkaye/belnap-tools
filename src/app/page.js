import * as IndexRes from "./Index.res.mjs"

export const metadata = {
  title: "Belnap tools",
  description: "Tools for experimenting with the Belnap interpretation.",
}

// Note:
// We need to wrap the make call with
// a Fast-Refresh conform function name,
// (in this case, uppercased first letter)
//
// If you don't do this, your Fast-Refresh will
// not work!
export default function Page(props) {
  return <IndexRes.make {...props} />
}
