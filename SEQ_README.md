# Seq (Lazy Sequences)

## General notes

!!!!! Check if recursion is a problem !!!!!
mapi or rely on indexed first?
to iterator
Use Option instead of Empty and Next; or enabpe mapIfNext
Remove recursion problems
change code to xxs and xs

### Ideas

fromIterable, nextChunk, asNonEmpty, peekThen

## TC39 Proposal

[Iterator helpers](https://github.com/tc39/proposal-iterator-helpers)

### Supported

map, filter, take, drop, flatMap, reduce, toArray, forEach, some, every, find

### Not supported

from (iterator)

## oCaml

[oCaml Module Seq](https://v2.ocaml.org/api/Seq.html)

### Supported

isEmpty, uncons, length, iter, fold_left, iteri, fold_lefti, for_all, exists, find, find_map, compare, empty, return, cons, init, unfold, equal, repeat, forever, iterate, cycle, map, mapi, filter, filter_map, take, drop, take_while, drop_while, memoize, append, concat, flat_map, zip, sorted_merge, scan, map2, product, concat_map, interleave

### Not supported

iter2, fold_left2, for_all2, exists2, group, once, map_product, unzip, partition_map, partition, ints

## F#

[F# Seq module](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-seqmodule.html#cache)

### Supported

allPairs, append, cache, choose, chunkBySize, collect, compareWith, concat, empty, exists, filter, fold, forAll, head, indexed, init, initInfinite, isEmpty, iter, iteri, last, length, map, map2, mapFold, mapi, maxBy, minBy, ofArray, ofList, pairwise, pick, replicate, scan, singleton, skip, skipWhile, tail, takeWhile, toArray, truncate, tryExactlyOne, tryFind, tryHead, tryLast, tryPick, unfold, where, windowed, zip

### Not supported

average, averageBy, cast, contains, countBy, delay, distinctBy, exactlyOne, except, exists2, findBack, findIndex, fold2, foldBack, foldBack2, forAll2, groupBy, insertAt, insertManyAt, item (nth), iter2, iteri2, map3, mapFoldBack, mapi2, max, min, permute, readonly, **reduce** (simplified fold throw if empty), reduceBack, removeAt, removeManyAt, rev, scanBack, sort, sortBy, sortByDescending, sortDescending, sortWith, splitInto, sum, sumBy, toList, transpose, take (throws like takeExactly), tryFindBack, tryFindIndex, tryFindIndexBack, tryItem, updateAt, zip3

## Rust

[Rust std::iter](https://doc.rust-lang.org/stable/std/iter/) and [more docs](https://doc.rust-lang.org/std/iter/trait.Iterator.html)

### Supported

all, any, array_chunks, chain, cmp_by, count, cycle, enumerate, eq_by, filter, filter_map, find, find_map, flat_map, flatten, fold, for_each, inspect, intersperse, is_sorted_by, last, map, max_by, min_by, partial_cmp_by, product, scan, skip, skip_while, take, take_while, zip

### Not supported

advance_by, cloned, cmp, collect, collect_into, copied, eq, fuse, ge, gt, intersperseWith, partitioned, try_fold, is_sorted, is_sorted_by_key, le, lt, map_while, max, min, ne, next_chunk, nth, partial_cmp, partition, partition_in_place, peekable, position, **reduce** ( to option), rev, rPosition, size_hint, step_by, sum, try_collect, try_find, try_fold, try_for_each, try_reduce, unzip, some kind of **remainder** ability

## Itertools (Python)

[Reference](https://docs.python.org/3/library/itertools.html)

### Supported

count, cycle, repeat, accumulate, chain, dropwhile, filterfalse, pairwise, takewhile, zip_longest, product,

### Not supported

compress, groupby, islice, starmap, tee, permutations, combinations, combinations_with_replacement, recipes: all_equal, subslices

## Itertools (Javascript)

[Reference for JS itertools](https://github.com/iter-tools/iter-tools/blob/v7.5.0/API.md)

### Supported

range, drop, dropWhile, enumerate, filter, flat, flatMap, interpose, map, prepend, take, takeWhile, tap, window, batch, collate, concat, join, zip, zippAll, deepEqual, every, find, findBest, first, isEmpty, reduce, size, some, takeLast, fork, arrayFrom, forEach, toString, toArray, firstHighest, firstLowest, roundRobin, windowBehind, windowAhead

### Not supported

range (with step), repeat (constant), objectEntries, objectKeys, objectValues, append (one value), distinct, interposeSeq, reverse, slice, takeSorted, bisect, split, splitGroups, splitOn, splitWhen, compress, joinWith, firstOr, includes, includesAny, **many async flavors**, isSorted, startsWith, startsWithAny, str, takeLastOr, objectFrom, toObject, lastHighest, lastLowest

## MoreLINQ

[MoreLINQ on Github](https://github.com/morelinq/MoreLINQ)

### Supported

takeUntil
