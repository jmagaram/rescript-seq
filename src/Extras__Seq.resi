type t<'a>

// =========
// Construct
// =========
let empty: t<'a>
let singleton: 'a => t<'a>
let unfold: ('seed, 'seed => option<('a, 'seed)>) => t<'a>
let init: (~count: int, ~initializer: (~index: int) => 'a) => t<'a>
let replicate: (~count: int, ~value: 'a) => t<'a>
let infinite: (unit => 'a) => t<'a>
// range, step, etc.

// =========
// Transform
// =========
let append: (t<'a>, t<'a>) => t<'a>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>
let indexed: t<'a> => t<('a, int)>
let filter: (t<'a>, 'a => bool) => t<'a>
let take: (t<'a>, int) => t<'a>
let drop: (t<'a>, int) => t<'a>
// let mapi: (t<'a>, (~value: 'a, ~index: int) => 'b) => t<'b>
// takewhile

// =======
// Consume
// =======
let fold: (t<'a>, 'b, ('b, 'a) => 'b) => 'b
let toArray: t<'a> => array<'a>
let toReversedList: t<'a> => Belt.List.t<'a>
let forEach: (t<'a>, 'a => unit) => unit
let some: (t<'a>, 'a => bool) => bool
let everyOrEmpty: (t<'a>, 'a => bool) => bool
let find: (t<'a>, 'a => bool) => option<'a>
// tap

/*
Check if recursion is a problem

*/
// =====
// NOTES
// =====

// get all the tc39
// get all the oCaml
// to iterator
// from array too

// TC39
// https://github.com/tc39/proposal-iterator-helpers
// map - YES
// filter - YES
// take - YES
// drop - YES
// flatMap - YES
// reduce - YES as fold, initial value first
// toArray - YES
// forEach - YES
// some - YES
// every - YES as everyOrEmpty
// find - YES
// from - NO an object @@iterator
