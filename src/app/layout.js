"server-only"

import "./globals.css"

import * as LayoutRes from "./MainLayout.res.mjs"

export default function Layout(props) {
  return (
    <html lang="en">
      <body>
        <main>
          <LayoutRes.make {...props} />
        </main>
      </body>
    </html>
  )
}
