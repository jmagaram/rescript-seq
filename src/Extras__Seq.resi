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
let fromArray: array<'a> => t<'a>
let iterate: ('a, 'a => 'a) => t<'a>

// =========
// Transform
// =========
let append: (t<'a>, t<'a>) => t<'a>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>
let mapi: (t<'a>, (~value: 'a, ~index: int) => 'b) => t<'b>
let indexed: t<'a> => t<('a, int)>
let filter: (t<'a>, 'a => bool) => t<'a>
let filterMap: (t<'a>, 'a => option<'b>) => t<'b>
let take: (t<'a>, int) => t<'a>
let drop: (t<'a>, int) => t<'a>
let takeWhile: (t<'a>, 'a => bool) => t<'a>
let dropWhile: (t<'a>, 'a => bool) => t<'a>
let zip: (t<'a>, t<'b>) => t<('a, 'b)>
let zipLongest: (t<'a>, t<'b>) => t<(option<'a>, option<'b>)>
let scani: (t<'a>, ~zero: 'b, (~sum: 'b, ~value: 'a, ~index: int) => 'b) => t<'b>
let flatten: t<t<'a>> => t<'a>
let map2: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

// =======
// Consume
// =======
let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b
let reducei: (t<'a>, 'b, (~sum: 'b, ~value: 'a, ~index: int) => 'b) => 'b
let toArray: t<'a> => array<'a>
let toReversedList: t<'a> => Belt.List.t<'a>
let forEach: (t<'a>, 'a => unit) => unit
let forEachi: (t<'a>, (~value: 'a, ~index: int) => unit) => unit
let some: (t<'a>, 'a => bool) => bool
let everyOrEmpty: (t<'a>, 'a => bool) => bool
let find: (t<'a>, 'a => bool) => option<'a>
let findMap: (t<'a>, 'a => option<'b>) => option<'b>
let length: t<'a> => int
let equals: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

/*
=====
NOTES
=====
Check if recursion is a problem
Simple ranges with steps, up and down, inclusive or exclusive
mapi or rely on indexed first?
takeWhile
tap

// get all the tc39
// get all the oCaml
// to iterator
// from array too

=============
TC39 Proposal
=============
https://github.com/tc39/proposal-iterator-helpers
map - YES
filter - YES
take - YES
drop - YES
flatMap - YES
reduce - YES as fold, initial value first
toArray - YES
forEach - YES
some - YES
every - YES as everyOrEmpty
find - YES
from - NO an object @@iterator

=====
oCaml
=====
isEmpty
uncons
length - YES
iter - YES as forEach
fold_left - YES as reduce
iteri - YES as forEachi
fold_lefti - YES as reducei
for_all - YES as everyOrEmpty
exists - YES as some
find - YES
find_map - YES
iter2
fold_left2
for_all2
exists2
equal - YES
compare
empty - YES
return - YES as singleton
cons
init - YES
unfold - YES
repeat - NO
forever - YES as infinite
iterate - YES
cycle

map - YES
mapi - YES
filter - YES
filter_map - YES
scan - NO (see scani)
take - YES
drop - YES
take_while - YES
drop_while - YES
group
memoize
once
transpose

append - YES
concat - YES as flatten
flat_map - YES as flatten
concat_map
zip (excludes longer items) - YES
map2
interlease
sorted_merge
product
map_product

unzip
split
partition_map
partition
ints

Should all constructor names be "from..."

// Array stuff like zip2by

*/
