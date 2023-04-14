type t<'a>

// Constructing sequences
let empty: t<'a>
let singleton: 'a => t<'a>
let unfold: ('seed, 'seed => option<('a, 'seed)>) => t<'a>
let init: (~count: int, ~initializer: (~index: int) => 'a) => t<'a>
let replicate: (~count: int, ~value: 'a) => t<'a>
// replicate!
// repeat and forever
// range, step, etc.

// Transform
let append: (t<'a>, t<'a>) => t<'a>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>

// Consume and export
let fold: (t<'a>, 'b, ('b, 'a) => 'b) => 'b
let toArray: t<'a> => array<'a>
let toReversedList: t<'a> => Belt.List.t<'a>
