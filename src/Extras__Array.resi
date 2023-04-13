let fromSeed: ('state, 'state => option<('a, 'state)>) => array<'a>
let fromOption: option<'a> => array<'a>
@val external fromOneValue: 'a => array<'a> = "Array.of"
let isEmpty: array<'a> => bool
let isNotEmpty: array<'a> => bool
let exactlyOneValue: array<'a> => option<'a>
let head: array<'a> => option<'a>
let last: array<'a> => option<'a>
let lastIndex: array<'a> => option<int>
let pairs: array<'a> => array<('a, 'a)>
let prepend: (array<'a>, array<'a>) => array<'a>
let filterSome: array<option<'a>> => array<'a>
let filterSomeWith: (array<'a>, ('a, int) => option<'b>) => array<'b>
let indexed: array<'a> => array<('a, int)>
let permute: (array<'a>, ~length: int) => array<array<'a>>
