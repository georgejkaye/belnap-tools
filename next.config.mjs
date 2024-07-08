import bsconfig from "./rescript.json" with { type: "json" }

const config = {
  output: "standalone",
  transpilePackages: ["rescript"].concat(bsconfig["bs-dependencies"]),
  pageExtensions: ["jsx", "js", "mjs"]
}

export default config
