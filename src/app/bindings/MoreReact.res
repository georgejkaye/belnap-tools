let map = (xs, fn) => React.array(Array.map(xs, fn))
let mapi = (xs, fn) => React.array(Array.mapWithIndex(xs, fn))

let mapfor = (i, fn) => React.array(Array.fromInitializer(~length=i, fn))
