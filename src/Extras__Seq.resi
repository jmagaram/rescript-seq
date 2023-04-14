type t<'a>

let empty: t<'a>
let singleton: 'a => t<'a>
let fold: (t<'a>, 'b, ('b, 'a) => 'b) => 'b
let toArray: t<'a> => array<'a>

// let makeRandom: (int, unit) => float

// let makeRandom1: (int, unit) => float

// type lazyRandom = unit => float
// let makeRandom2: int => lazyRandom

// let makeRandom3: int => (. unit) => float
