type t<'a>

// Constructing sequences
let empty: t<'a>
let singleton: 'a => t<'a>
let unfold: ('seed, 'seed => option<('a, 'seed)>) => t<'a>
let init: (~count: int, ~initializer: (~index: int) => 'a) => t<'a>
// range, step, etc.

// Consume and export
let fold: (t<'a>, 'b, ('b, 'a) => 'b) => 'b
let toArray: t<'a> => array<'a>
let toReversedList: t<'a> => Belt.List.t<'a>
