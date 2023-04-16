type t<'a>
exception ArgumentOfOfRange(string)

// =========
// Construct
// =========
let empty: t<'a>
let singleton: 'a => t<'a>
let unfold: ('seed, 'seed => option<('a, 'seed)>) => t<'a>
let init: (~count: int, ~initializer: (~index: int) => 'a) => t<'a>
let replicate: (~count: int, ~value: 'a) => t<'a>
let infinite: (unit => 'a) => t<'a>
let fromArray: (~start: int=?, ~end: int=?, array<'a>) => t<'a>
let iterate: ('a, 'a => 'a) => t<'a>
let cycle: t<'a> => t<'a>
let range: (~start: int, ~stop: int) => t<int>
// fromSet
// fromMap
// fromIterable

// =========
// Transform
// =========
let append: (t<'a>, t<'a>) => t<'a> // concat
let appendMany: (t<'a>, t<t<'a>>) => t<'a> // concatMany
let startWith: (t<'a>, 'a) => t<'a>
let startWithMany: (t<'a>, t<'a>) => t<'a> // not analogous!
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>
let mapi: (t<'a>, (~value: 'a, ~index: int) => 'b) => t<'b>
let indexed: t<'a> => t<('a, int)>
let filter: (t<'a>, 'a => bool) => t<'a>
let filteri: (t<'a>, (~value: 'a, ~index: int) => bool) => t<'a>
let filterMap: (t<'a>, 'a => option<'b>) => t<'b>
let filterSome: t<option<'a>> => t<'a> // filter Ok
let filterOk: t<result<'a, 'b>> => t<'a>
let take: (t<'a>, int) => t<'a>
let drop: (t<'a>, int) => t<'a>
let takeWhile: (t<'a>, 'a => bool) => t<'a>
let dropWhile: (t<'a>, 'a => bool) => t<'a>
let zip: (t<'a>, t<'b>) => t<('a, 'b)>
let zipLongest: (t<'a>, t<'b>) => t<(option<'a>, option<'b>)>
// rename scan?
let scani: (t<'a>, ~zero: 'b, (~sum: 'b, ~value: 'a, ~index: int) => 'b) => t<'b>
let flatten: t<t<'a>> => t<'a>
let map2: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>
let sortedMerge: (t<'a>, t<'a>, ('a, 'a) => int) => t<'a>
let cache: t<'a> => t<'a>
let tap: (t<'a>, 'a => unit) => t<'a>
let allPairs: (t<'a>, t<'b>) => t<('a, 'b)>
let chunkBySize: (t<'a>, int) => t<array<'a>>
let pairwise: t<'a> => t<('a, 'a)>
let windowed: (t<'a>, int) => t<array<'a>> // and pairwise; MUST have that many, no shorter
let intersperse: (t<'a>, 'a) => t<'a>

// =======
// Consume
// =======
let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b
let reducei: (t<'a>, 'b, (~sum: 'b, ~value: 'a, ~index: int) => 'b) => 'b
let toArray: t<'a> => array<'a>
let forEach: (t<'a>, 'a => unit) => unit
let forEachi: (t<'a>, (~value: 'a, ~index: int) => unit) => unit
let some: (t<'a>, 'a => bool) => bool
let everyOrEmpty: (t<'a>, 'a => bool) => bool
let find: (t<'a>, 'a => bool) => option<'a>
let findMap: (t<'a>, 'a => option<'b>) => option<'b>
let length: t<'a> => int
let isEmpty: t<'a> => bool
let equals: (t<'a>, t<'b>, ('a, 'b) => bool) => bool
let compare: (t<'a>, t<'b>, ('a, 'b) => int) => int
let headTail: t<'a> => option<('a, t<'a>)>
let minBy: (t<'a>, ('a, 'a) => int) => option<'a>
let maxBy: (t<'a>, ('a, 'a) => int) => option<'a>
let last: t<'a> => option<'a>

/*
=====
NOTES
=====
!!!!! Check if recursion is a problem !!!!!
Simple ranges with steps, up and down, inclusive or exclusive
mapi or rely on indexed first?
takeWhile

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
isEmpty - YES
uncons - YES
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
compare - YES
empty - YES
return - YES as singleton
cons - YES as startWith
init - YES
unfold - YES
repeat - NO
forever - YES as infinite
iterate - YES
cycle - YES

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
memoize - YES
once
transpose

append - YES
concat - YES as flatten
flat_map - YES as flatten
concat_map
zip (excludes longer items) - YES
map2 - YES
interleave
sorted_merge - YES
product - YES
map_product

unzip
split
partition_map
partition
ints

F#
https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-seqmodule.html#cache
===
allPairs - YES
chunkBySize - YES
countBy
exclude
findIndex and findLastIndex
insertAt
insertManyAt
lastItem - YES
map2 or map3
pairwise - YES
pick (findmap) - YES
removeAt
removeManyAt
reverse
tail (vs head)
transpose
findLast
windowed - YES

#Rust
intersperse - YES
arrayChunks - YES
fuse (stop at but include the first None)
isSorted
isSortedBy
last
nth
stepBy
nextChunk

#itertools
combinations
permutations
fromIterable
dropWhile
nth
partition
subslices
allEqual

Should all constructor names be "from..."
Use Option instead of Empty and Next; or enabpe mapIfNext
Remove recursion problems
Persistent collections?
Danger of compare to Empty, need isEmpty

// Array stuff like zip2by

*/
