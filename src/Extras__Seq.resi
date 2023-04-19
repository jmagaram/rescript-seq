type t<'a>
exception ArgumentOfOfRange(string)

// =========
// Construct
// =========
let empty: t<'a>
let singleton: 'a => t<'a>
let unfold: ('seed, 'seed => option<('a, 'seed)>) => t<'a>
let init: (~count: int, (~index: int) => 'a) => t<'a>
let replicate: (~count: int, ~value: 'a) => t<'a>
let infinite: (unit => 'a) => t<'a>
let iterate: ('a, 'a => 'a) => t<'a>
let cycle: t<'a> => t<'a>
let range: (~start: int, ~end: int) => t<int>
let fromArray: (~start: int=?, ~end: int=?, array<'a>) => t<'a>
let fromList: list<'a> => t<'a>
let fromOption: option<'a> => t<'a>
let fromString: string => t<string>
let cons: ('a, t<'a>) => t<'a>

// =========
// Transform
// =========
let startWith: (t<'a>, 'a) => t<'a>
let endWith: (t<'a>, 'a) => t<'a>
let prepend: (t<'a>, t<'a>) => t<'a>
let concat: (t<'a>, t<'a>) => t<'a>
let flatten: t<t<'a>> => t<'a>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>
let mapi: (t<'a>, (~value: 'a, ~index: int) => 'b) => t<'b>
let indexed: t<'a> => t<('a, int)>
let filter: (t<'a>, 'a => bool) => t<'a>
let filteri: (t<'a>, (~value: 'a, ~index: int) => bool) => t<'a>
let filterMap: (t<'a>, 'a => option<'b>) => t<'b>
let filterSome: t<option<'a>> => t<'a>
let filterOk: t<result<'a, 'b>> => t<'a>
let takeAtMost: (t<'a>, int) => t<'a>
let drop: (t<'a>, int) => t<'a>
let takeWhile: (t<'a>, 'a => bool) => t<'a>
let takeUntil: (t<'a>, 'a => bool) => t<'a>
let dropWhile: (t<'a>, 'a => bool) => t<'a>
let dropUntil: (t<'a>, 'a => bool) => t<'a>
let zip: (t<'a>, t<'b>) => t<('a, 'b)>
let zip3: (t<'a>, t<'b>, t<'c>) => t<('a, 'b, 'c)>
let zip4: (t<'a>, t<'b>, t<'c>, t<'d>) => t<('a, 'b, 'c, 'd)>
let zip5: (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>) => t<('a, 'b, 'c, 'd, 'e)>
let map2: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>
let map3: (t<'a>, t<'b>, t<'c>, ('a, 'b, 'c) => 'd) => t<'d>
let map4: (t<'a>, t<'b>, t<'c>, t<'d>, ('a, 'b, 'c, 'd) => 'e) => t<'e>
let map5: (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, ('a, 'b, 'c, 'd, 'e) => 'f) => t<'f>
let scan: (t<'a>, 'b, ('b, 'a) => 'b) => t<'b>
let scani: (t<'a>, ~zero: 'b, (~sum: 'b, ~value: 'a, ~index: int) => 'b) => t<'b>
let sortedMerge: (t<'a>, t<'a>, ('a, 'a) => int) => t<'a>
let cache: t<'a> => t<'a>
let tap: (t<'a>, 'a => unit) => t<'a>
let chunkBySize: (t<'a>, int) => t<array<'a>>
let pairwise: t<'a> => t<('a, 'a)>
let window: (t<'a>, int) => t<array<'a>>
let windowBehind: (t<'a>, int) => t<array<'a>>
let windowAhead: (t<'a>, int) => t<array<'a>>
let allPairs: (t<'a>, t<'b>) => t<('a, 'b)>
let intersperse: (t<'a>, 'a) => t<'a>
let interleave: (t<'a>, t<'a>) => t<'a>

// =======
// Consume
// =======
let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b
let reducei: (t<'a>, 'b, (~sum: 'b, ~value: 'a, ~index: int) => 'b) => 'b
let forEach: (t<'a>, 'a => unit) => unit
let forEachi: (t<'a>, (~value: 'a, ~index: int) => unit) => unit
let some: (t<'a>, 'a => bool) => bool
let everyOrEmpty: (t<'a>, 'a => bool) => bool
let find: (t<'a>, 'a => bool) => option<'a>
let findMap: (t<'a>, 'a => option<'b>) => option<'b>
let findMapi: (t<'a>, (~value: 'a, ~index: int) => option<'b>) => option<'b>
let length: t<'a> => int
let isEmpty: t<'a> => bool
let isSortedBy: (t<'a>, ('a, 'a) => int) => bool
let equals: (t<'a>, t<'a>, ('a, 'a) => bool) => bool
let compare: (t<'a>, t<'a>, ('a, 'a) => int) => int
let head: t<'a> => option<'a>
let tail: t<'a> => t<'a>
let headTail: t<'a> => option<('a, t<'a>)>
let minBy: (t<'a>, ('a, 'a) => int) => option<'a>
let maxBy: (t<'a>, ('a, 'a) => int) => option<'a>
let last: t<'a> => option<'a>
let toArray: t<'a> => array<'a>
let toString: t<string> => string
let toExactlyOne: t<'a> => option<'a>
let toOption: t<'a> => option<t<'a>>
let allOk: t<result<'ok, 'err>> => result<t<'ok>, 'err>
let allSome: t<option<'a>> => option<t<'a>>
