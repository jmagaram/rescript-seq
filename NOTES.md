# Notes

General:

- [To do](#to-do)
- [Miscellaneous notes](#miscellaneous-notes)

Other sequence/iterable packages:

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

## To do

## Miscellaneous notes

### Terminology for `concat`

- F# `concat` is flatten and `append` is just 1
- OCaml `concat` is flatten and `append` is just 1
- Haskell `concat` is flatten and `+` is just 1
- JavaScript arrays `concat` is one or more, and separate `flat` function
- .net Enumerable `concat` is just one
- Rust is `flatten` for many and `chain` for just one

Ok, sticking with `concat` for JavaScript familiarity, even though it conflicts with sequences in other functional languages. But adding synonym of `append` for since it goes nicely with `prepend`.

### Indexed versions of functions like `mapi`

Sometimes it is useful to have the index of an item when performing a computation. This can be accomplished by calling the `indexed` function before calling the function that needs the index. This is how it is done in `F#`. `Rust` has an `enumerate` function. So indexed versions aren't as important as they would be with non-lazy `array` functions where adding the index regenerates the entire array. Indexed versions are useful in a couple cases: (1) where developers expect it from experience with JavaScript `array` functions, (2) when the user is likely going to NOT want the index after they have performed some kind of transformation, because then they have to first call `indexed`, and then after the transformation, do a `map` to get rid of it. This is the case with `filter`.

### DistinctBy, other set operations

There are many useful "set-based" functions like `union`, `intersect`, `distinct`, `except`, `countBy` that could be built if we had a...

```rescript
  type set<'a, 'b> = {
    empty: unit => 'b,
    has: ('b, 'a) => bool,
    add: ('b, 'a) => 'b,
  }
```

Also it is useful to summarize the entire data set with a `groupBy`, do `join`, etc.

I got a lazy distinctBy working where the user provides a comparison function `('a,'a)=>int`. The code relies on `Belt.Set` under-the-hood. `Belt.Set` relies on a hardcoded module for each type you want to put in the set, so I made it a type `unknown` and did some `Obj.magic` to make it work. My implementation was hardcoded to a specific set and equality test. Better performance might be possible using hash equality or the built-in JavaScript `Set`. Also if the function is meant to be lazy, an immutable set is helpful.

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

range, drop, dropWhile, enumerate, filter, flat, flatMap, interpose, map, prepend, take, takeWhile, tap, window, batch, collate, concat, join, zip, deepEqual, every, find, findBest, first, isEmpty, reduce, size, some, takeLast, fork, arrayFrom, forEach, toString, toArray, firstHighest, firstLowest, repeat, reverse, roundRobin, isSorted

### Not supported

range (with step), objectEntries, objectKeys, objectValues, append (one value), distinct, interposeSeq, slice, takeSorted, bisect, split, splitGroups, splitOn, splitWhen, compress, joinWith, firstOr, includes, includesAny, **many async flavors**, startsWith, startsWithAny, str, takeLastOr, objectFrom, toObject, lastHighest, lastLowest, zipAll, windowAhead, windowBehind

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
