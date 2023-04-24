# Research and notes

- [TC39 Proposal](#tc39-proposal)
- [OCaml](#ocaml)
- [F#](#f)
- [Rust](#rust)
- [Python Itertools](#itertools-python)
- [Itertools for Javascript](#itertools-javascript)
- [More LINQ](#morelinq)
- [Immutable.js Seq](#immutablejs-seq)

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

isEmpty, uncons, length, iter, fold_left, iteri, fold_lefti, for_all, exists, find, find_map, compare, empty, return, cons, init, unfold, equal, repeat, forever, iterate, cycle, map, mapi, filter, filter_map, take, drop, take_while, drop_while, memoize, append, concat, flat_map, zip, sorted_merge, scan, product, concat_map, interleave, map2

### Not supported

iter2, fold_left2, for_all2, exists2, group, once, map_product, unzip, partition_map, partition, ints

## F#

[F# Seq module](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-seqmodule.html#cache)
[Code](https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/seq.fs)

### Supported

allPairs, append, cache, choose, chunkBySize, collect, compareWith, concat, delay, empty, exists, filter, fold, forAll, head, indexed, init, initInfinite, isEmpty, iter, iteri, last, length, map, map2, map3, mapFold, mapi, maxBy, minBy, ofArray, ofList, pairwise, pick, replicate, rev, scan, singleton, skip, skipWhile, sortWith, tail, takeWhile, toArray, truncate, tryExactlyOne, tryFind, tryHead, tryLast, tryPick, unfold, where, windowed, zip, zip3

### Not supported

average, averageBy, cast, contains, countBy, distinctBy, exactlyOne, except, exists2, findBack, findIndex, fold2, foldBack, foldBack2, forAll2, groupBy, insertAt, insertManyAt, item (nth), iter2, iteri2, mapFoldBack, mapi2, max, min, permute, readonly, **reduce** (simplified fold throw if empty), reduceBack, removeAt, removeManyAt, scanBack, sort, sortBy, sortByDescending, sortDescending, sortWith, splitInto, sum, sumBy, toList, transpose, take (throws like takeExactly), tryFindBack, tryFindIndex, tryFindIndexBack, tryItem, updateAt

## Rust

[Rust std::iter](https://doc.rust-lang.org/stable/std/iter/) and [more docs](https://doc.rust-lang.org/std/iter/trait.Iterator.html)

[More tools](https://docs.rs/itertools/latest/itertools/) **need to look!**

### Supported

all, any, array_chunks, chain, cmp_by, count, cycle, enumerate, eq_by, filter, filter_map, find, find_map, flat_map, flatten, fold, for_each, inspect, intersperse, intersperseWith, is_sorted_by, last, map, max_by, min_by, partial_cmp_by, product, rev, scan, skip, skip_while, take, take_while, zip

### Not supported

advance_by, cloned, cmp, collect, collect_into, copied, eq, fuse, ge, gt, partitioned, try_fold, is_sorted, is_sorted_by_key, le, lt, map_while, max, min, ne, next_chunk, nth, partial_cmp, partition, partition_in_place, peekable, position, **reduce** ( to option), rPosition, size_hint, step_by, sum, try_collect, try_find, try_fold, try_for_each, try_reduce, unzip, some kind of **remainder** ability

## Itertools (Python)

[Reference](https://docs.python.org/3/library/itertools.html)

### Supported

cycle, repeat, accumulate, chain, dropwhile, filterfalse, pairwise, takewhile, zip_longest, product,

### Not supported

count, compress, groupby, islice, starmap, tee, permutations, combinations, combinations_with_replacement, recipes: all_equal, subslices

## Itertools (Javascript)

[Reference for JS itertools](https://github.com/iter-tools/iter-tools/blob/v7.5.0/API.md)

### Supported

range, drop, dropWhile, enumerate, filter, flat, flatMap, interpose, map, prepend, take, takeWhile, tap, window, batch, collate, concat, join, zip, deepEqual, every, find, findBest, first, isEmpty, reduce, size, some, takeLast, fork, arrayFrom, forEach, toString, toArray, firstHighest, firstLowest, repeat, reverse, roundRobin, windowBehind, windowAhead, isSorted

### Not supported

range (with step), objectEntries, objectKeys, objectValues, append (one value), distinct, interposeSeq, slice, takeSorted, bisect, split, splitGroups, splitOn, splitWhen, compress, joinWith, firstOr, includes, includesAny, **many async flavors**, startsWith, startsWithAny, str, takeLastOr, objectFrom, toObject, lastHighest, lastLowest, zipAll

## MoreLINQ

[MoreLINQ on Github](https://github.com/morelinq/MoreLINQ)

Not investigated everything yet

### Supported

takeUntil, dropUntil, consume, fallbackIfEmpty

### Not supported

endsWith, flatten (recursive), tagFirstLast, toMap, toObject

## Immutable.js Seq

Not investigated everything yet
https://immutable-js.com/docs/v4.3.0/Seq/
