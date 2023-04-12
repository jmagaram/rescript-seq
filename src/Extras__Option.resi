let isSomeAnd: (option<'a>, 'a => bool) => bool
let isNoneOr: (option<'a>, 'a => bool) => bool
let toArray: option<'a> => array<'a>
let concat: (option<'a>, option<'a>, ('a, 'a) => 'a) => option<'a>
let fold: (option<'a>, ('b, 'a) => 'b, 'b) => 'b
let foldBack: ('a, ('b, 'a) => 'a, option<'b>) => 'a
let fromOk: result<'a, 'b> => option<'a>
let fromError: result<'a, 'b> => option<'b>
let fromTryCatch: (unit => 'a) => option<'a>
let map2: (option<'a>, option<'b>, ('a, 'b) => 'r) => option<'r>
let map3: (option<'a>, option<'b>, option<'c>, ('a, 'b, 'c) => 'r) => option<'r>
let map4: (option<'a>, option<'b>, option<'c>, option<'d>, ('a, 'b, 'c, 'd) => 'r) => option<'r>
let flatten: option<option<'a>> => option<'a>
let orElseWith: (option<'a>, unit => option<'a>) => option<'a>
