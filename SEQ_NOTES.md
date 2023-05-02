# Research and notes

- [Notes, ideas, etc.](#notes)
- [TC39 Proposal](#tc39-proposal)
- [OCaml](#ocaml)
- [F#](#f)
- [.net Enumerable](#net-enumerable)
- [Rust](#rust)
- [Python Itertools](#itertools-python)
- [Itertools for Javascript](#itertools-javascript)
- [More LINQ](#morelinq)
- [Immutable.js Seq](#immutablejs-seq)
- [Haskell List](#haskell)
- [Racket](#racket)

## Notes

- Look again at `findMapi` and other indexers. Do we need them or just use `indexed` first?
- Move Seq into a separate project?
- Should any functions return a result rather than option?
- Skip last done lazily?
- `takeAtMost` and `dropAtMost`? Which name should be used?
- `orElse` may not be best name though it is similar to `Option.orElse`. `orElseIfEmpty`, `ifEmptyThen`, `fallbackIfEmpty`.

### About indexed versions of functions like `mapi`

Sometimes it is useful to have the index of an item when performing a computation. This can be accomplished by calling the `indexed` function before calling the function that needs the index. This is how it is done in `F#`. `Rust` has an `enumerate` function. So indexed versions aren't as important as they would be with non-lazy `array` functions where adding the index regenerates the entire array. Indexed versions are useful in a few cases: (1) where developers expect it from experience with JavaScript `array` functions, (2) when the user is likely going to NOT want the index after they have performed some kind of transformation, because then they have to fist call `indexed`, and then after the transformation, do a `map(snd)` to get rid of it. This is the case with `filter`.

Indexed versions are especially uninteresting when the developer is going to discard the index quickly, like when a `map`. a For sequences then the indexer versions are mostly useful when the developer is going to discard the index

### Naming of "uncons"

`uncons` used in functional programming often and opposite of the `cons`. `headTail` is annoying to use. `match`? `splitAt`? Or how about `next` to indicate it is consuming one item?

### Chunking and splitting

Use cases for sorted sequences:

- Chunk by key and then in each chunk, calculate minimum, length, or just an array of each. Could group by month, even/odd, status, etc.
- Find sequences of increasing numbers
- Find distinct adjacent values

Implementation thoughts:

- Naming needs to distinguish between something that summarizes across entire sequence and adjacent chunks. `groupBy` probably implies the whole thing. Also, `partition` is used in Belt and OCaml Seq for the whole thing.
- Very often helpful to see previous value when making a split decision. This value can be stored in the previous chunk, though, as a key perhaps.
- Maximum number of splits might be useful and is part of `MoreLINQ`

F# has a `chunkBySize`. In F# `groupBy` digests the entire sequence into unique keys.

Haskell has a `group` function that creates lists of lists such that if you concatenate them all together you end up with the original.

Naming ideas: `chunkAndReduce`, `chunkReduce`, `split`, `splitReduce`, `groupAdjacent`, `adjacentGroupBy`, `groupAdjacent`, `reduceAdjacent`, `groupWith`. `group` is a great word. `divide`, `segment`, `collect`. `summarizeAdjacent`. `cluster`. `chunkInto`. `split` focuses on the act of splitting, not on what is left over. Maybe name the array chunk one separate like `arrayChunksBySize` and then have `chunkBy` and `chunkByKey`.

### DistinctBy, other set operations

Many useful functions like `union`, `intersect`, `distinct`, `except`, `countBy` could be built if we had a...

```rescript
  type set<'a, 'b> = {
    empty: unit => 'b,
    has: ('b, 'a) => bool,
    add: ('b, 'a) => 'b,
  }
```

It is generally useful to filter out duplicate elements on demand. I got this working where the user provides a comparison function `('a,'a)=>int` which is really easy to do. The code relies on `Belt.Set` under-the-hood. `Belt.Set` relies on a hardcoded module for each type you want to put in the set, so I made it a type `unknown` and did some `Obj.magic` to make it work. There are some things to think about.

My implementation was hardcoded to a specific equality test. Better performance might be possible using hash equality or the built-in JavaScript `Set`. But adding this level of customization is messy. We'd need separate functions for each kind of test, like `distinctByHash`, `distinctByComparison`, etc. Or one `distinctBy` that takes an `equality<'a>` to determine what path to take.

It only works with `Belt.Set` because that is an immutable set. I generated unique items on demand, which is pretty cool. If a mutable set it used, the sequence can't be iterated more than once and it is probably better to generate all unique items at once.

`F#` has this feature. They eagerly consume all the items and use a mutable set.

```rescript
let distinctBy = (xx: t<'a>, compare: ('a, 'a) => int) => {
  module Set = Belt.Set
  module Comparator = Belt.Id.MakeComparable({
    type t = unknown
    let cmp = (a: unknown, b: unknown) => compare((a->Obj.magic: 'a), (b->Obj.magic: 'a))
  })
  xx
  ->scan((None, Set.make(~id=module(Comparator))), ((_, set), x) => {
    switch set->Set.has((x->Obj.magic: unknown)) {
    | true => (None, set)
    | false => (Some(x), set->Set.add((x->Obj.magic: unknown)))
    }
  })
  ->filterMap(((x, _set)) => x)
}

```

### "sumBy" and "prefixSum"

`fold` is powerful since you can supply a default `zero` value and compute a type that is different than what you start with. But a simpler version, where the initial value is the first value, is useful too. F# has this. They call it `reduce` vs. `fold`. In JavaScript `Array.reduce` you can omit the initial parameter. Haskell and other packages have this simplified flavor of fold. Also, just like `fold` is related to `scan`, it is useful to have a function that returns running sums, an inclusive scan.

Don't want the term to be too number-focused, since you can accumulate strings, arrays, etc.

`minBy` and `maxBy` already exist. The result type is the same as the source type.

Possible names for complete sum: `reduceFromFirst`, `sumBy`, `totalBy`.

Possible names for running sum: `prefixSum`, `cumulativeSum`, `runningTotal`, `partialSums`, `cumsum` (used in MATLAB, R, and Julia), `accumulate`.

In F#, `sumBy` allows you to turn each item into a number, and then adds them up using the default zero. Sometimes `By` suffix means a projection, but in this case you'd want to use `reduce`.

See [prefix sum](https://en.wikipedia.org/wiki/Prefix_sum)

## TC39 Proposal

[Iterator helpers](https://github.com/tc39/proposal-iterator-helpers)

### Supported

map, filter, take, drop, flatMap, reduce, toArray, forEach, some, every, find

### Not supported

from (iterator)

## oCaml

[oCaml Module Seq](https://v2.ocaml.org/api/Seq.html)
[Source code](https://github.com/ocaml/ocaml/blob/trunk/stdlib/seq.ml)

### Supported

isEmpty, uncons, length, iter, fold_left, iteri, fold_lefti, for_all, exists, find, find_map, compare, empty, return, cons, init, unfold, equal, repeat, forever, iterate, cycle, map, mapi, filter, filter_map, take, drop, take_while, drop_while, memoize, append, concat, flat_map, zip, sorted_merge, scan, product, concat_map, interleave, map2, once, onceWith

### Not supported

iter2, fold_left2, for_all2, exists2, group, map_product, unzip, partition_map, partition, ints

## F#

[F# Seq module](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-seqmodule.html#cache)
[Code](https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/seq.fs)

### Supported

allPairs, append, cache, choose, chunkBySize, collect, compareWith, concat, delay, empty, exists, filter, fold, forAll, head, indexed, init, initInfinite, isEmpty, iter, iteri, last, length, map, map2, map3, mapFold, mapi, maxBy, minBy, ofArray, ofList, pairwise, pick, replicate, reduce, rev, scan, singleton, skip, skipWhile, sortWith, tail, takeWhile, toArray, toList, truncate, tryExactlyOne, tryFind, tryHead, tryLast, tryPick, unfold, where, windowed, zip, zip3

### Not supported

average, averageBy, cast, contains, countBy, distinctBy, exactlyOne, except, exists2, findBack, findIndex, fold2, foldBack, foldBack2, forAll2, groupBy, insertAt, insertManyAt, item (nth), iter2, iteri2, mapFoldBack, mapi2, max, min, permute, readonly, reduceBack, removeAt, removeManyAt, scanBack, sort, sortBy, sortByDescending, sortDescending, sortWith, splitInto, sum, sumBy, transpose, take (throws like takeExactly), tryFindBack, tryFindIndex, tryFindIndexBack, tryItem, updateAt

## .net Enumerable

[.net Enumerable](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable?view=net-7.0)

Interesting that `take` is really `takeAtMost` and `skip` is really `skipAtMost` while for F# these throw exceptions if there aren't enough elements.

## Rust

[Rust std::iter](https://doc.rust-lang.org/stable/std/iter/) and [more docs](https://doc.rust-lang.org/std/iter/trait.Iterator.html)

[More tools](https://docs.rs/itertools/latest/itertools/) **need to look!**

### Supported

all, any, array_chunks, chain, cmp_by, count, cycle, enumerate, eq_by, filter, filter_map, find, find_map, flat_map, flatten, fold, for_each, inspect, intersperse, intersperseWith, is_sorted_by, last, map, max_by, min_by, partial_cmp_by, product, reduce, rev, scan, skip, skip_while, take, take_while, zip

### Not supported

advance_by, cloned, cmp, collect, collect_into, copied, eq, fuse, ge, gt, partitioned, try_fold, is_sorted, is_sorted_by_key, le, lt, map_while, max, min, ne, next_chunk, nth, partial_cmp, partition, partition_in_place, peekable, position, rPosition, size_hint, step_by, sum, try_collect, try_find, try_fold, try_for_each, try_reduce, unzip, some kind of remainder ability

## Itertools (Python)

[Reference](https://docs.python.org/3/library/itertools.html)

### Supported

cycle, repeat, accumulate, chain, dropwhile, filterfalse, pairwise, takewhile, zip_longest, product, permutations, combinations

### Not supported

count, compress, groupby, islice, starmap, tee, combinations_with_replacement, recipes: all_equal, subslices

## Itertools (Javascript)

[Reference for JS itertools](https://github.com/iter-tools/iter-tools/blob/v7.5.0/API.md)

### Supported

range, drop, dropWhile, enumerate, filter, flat, flatMap, interpose, map, prepend, take, takeWhile, tap, window, batch, collate, concat, join, zip, deepEqual, every, find, findBest, first, isEmpty, reduce, size, some, takeLast, fork, arrayFrom, forEach, toString, toArray, firstHighest, firstLowest, repeat, reverse, roundRobin, windowBehind, windowAhead, isSorted

### Not supported

range (with step), objectEntries, objectKeys, objectValues, append (one value), distinct, interposeSeq, slice, takeSorted, bisect, split, splitGroups, splitOn, splitWhen, compress, joinWith, firstOr, includes, includesAny, **many async flavors**, startsWith, startsWithAny, str, takeLastOr, objectFrom, toObject, lastHighest, lastLowest, zipAll

## MoreLINQ

[MoreLINQ on Github](https://github.com/morelinq/MoreLINQ)

[Video series on many of these](https://markheath.net/category/MoreLINQ)
Not investigated everything yet

### Supported

takeUntil, dropUntil, consume, fallbackIfEmpty

### Not supported

endsWith, flatten (recursive), tagFirstLast, toMap, toObject

## Immutable.js Seq

Not investigated everything yet
https://immutable-js.com/docs/v4.3.0/Seq/

## Haskell

https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html

`prefix` and `drop` do not throw.

append, head, last, tail, init (all elements except for the last), uncons, length, map, reverse, intersperse, intercalate, transpose, subsequences (combinations), permutations, foldl, foldll (no initial value), foldr, foldrl, concat, concatMap, and, or, any, all, sum, product, maximum, minimum, scanl, scanr, **mapAccumL** (is this like the F# one?), iterate, repeat, replicate, cycle, unfold, take, drop, splitAt, takeWhile, dropWhile, dropWhileEnd, span, break, stripPrefix, group, inits, tails, isPrefixOf, isSuffixOf, isInfixOf, isSubsequenceOf, find, filter, partition, elemIndex, elemIndices, findIndex, findIndices, zip, zip3, etc. zipWith, zipWith3, etc. unzip, lines, union, intersect, sortOn, insertBy,

## Racket

https://docs.racket-lang.org/seq/index.html
`deduplicate` generates a list not a sequence

by (every nth element), rest, init (all but the last)
