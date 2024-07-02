/** @type {import('tailwindcss').Config} */
export default {
  // Specify the paths to all of the template files in your project
  content: [
    "./src/pages/**/*.{js,mjs}",
    "./src/components/**/*.{js,mjs}",
    "./src/app/**/*.{js,mjs}",
  ],
  theme: {
    extend: {
      colors: {
        accent: "#000b9e",
      },
    },
  },
  plugins: [],
}
