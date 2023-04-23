# Lazy Sequences for ReScript

A _sequence_ is a list whose elements are computed only on demand. Sequences are produced and transformed lazily (one element at a time) rather than eagerly (all at once). This allows constructing conceptually infinite sequences. When your data is an `array`, as it almost always is, a single `fromArray` function wraps it in a sequence and makes it possible to analyze and transform it with far more flexibility and power than what is possible using only the built-in array functions. A sequence can also provide better performance than an `array` when not all elements are used. Sequences are similar to [JavaScript iterables](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols).

Sequences are an important data structure in [F# (Seq module)](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-seqmodule.html) , [OCaml (Seq module)](https://v2.ocaml.org/api/Seq.html), [Rust std::iter](https://doc.rust-lang.org/std/iter/trait.Iterator.html), [C# .net IEnumerable<T>](https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.ienumerable-1?view=net-8.0), and [Python (itertools)](https://docs.python.org/3/library/itertools.html). There are many JavaScript libraries for consuming iterables as well.

**This is a full-featured library for creating and consuming sequences in ReScript.**

Highlights:

- Enables more elegant and concise solutions than using imperative code and arrays
- All APIs documented; some with code examples
- Comprehensive test suite
- Written 100% in ReScript. Look at the tests and code to gain deeper understanding of how it works.
- Full [suite of > 90 functions](#functions) based on researching other libraries and languages
- **Build sequences** using `fromArray`, `range`, `unfold`, `cycle`, `repeat` and others. This enables functionality similar to [JavaScript generators](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Generator).
- **Transform** sequences with `map`, `filter`, `takeAtMost`, `dropWhile`, `scan`, `tap`, `window`, and others.
- **Combine** multiple sequences with `zip`, `zip3`, `map2`, `map3`, `sortedMerge`, `interleave` and others.
- **Calculate** values with `reduce`, `some`, `findMap`, `isSortedBy`, `minBy`, `toArray` and others.
- Ready for your contributions! :two_men_holding_hands: There are many other useful functions, and it would be great to have `async` versions.

See code [examples](src/Extras__SeqSamples.res) to get a sense for how `Seq` is used and what is possible.

## Install

This is part of the [rescript-extras package](README.md) right now; see install instructions there.

## Functions

```rescript
type t<'a> // A lazy sequence of `a`

// Construct

let characters: Js.String2.t => t<Js.String2.t>
let cons: ('a, t<'a>) => t<'a>
let cycle: t<'a> => t<'a>
let reverse: (unit, t<'a>) => t<'a>
let empty: t<'a>
let endWith: (t<'a>, 'a) => t<'a>
let forever: 'a => t<'a>
let foreverWith: (unit => 'a) => t<'a>
let fromArray: (~start: int=?, ~end: int=?, array<'a>) => t<'a>
let fromList: list<'a> => t<'a>
let fromOption: option<'a> => t<'a>
let init: (int, int => 'a) => t<'a>
let iterate: ('a, 'a => 'a) => t<'a>
let range: (int, int) => t<int>
let rangeMap: (int, int, int => 'a) => t<'a>
let repeat: (int, 'a) => t<'a>
let repeatWith: (int, unit => 'a) => t<'a>
let singleton: 'a => t<'a>
let startWith: (t<'a>, 'a) => t<'a>
let unfold: ('a, 'a => option<('b, 'a)>) => t<'b>

// Transform

let cache: t<'a> => t<'a>
let chunkBySize: (t<'a>, int) => t<array<'a>>
let drop: (t<'a>, int) => t<'a>
let dropUntil: (t<'a>, 'a => bool) => t<'a>
let dropWhile: (t<'a>, 'a => bool) => t<'a>
let filter: (t<'a>, 'a => bool) => t<'a>
let filteri: (t<'a>, ('a, int) => bool) => t<'a>
let filterMap: (t<'a>, 'a => option<'b>) => t<'b>
let filterMapi: (t<'a>, ('a, int) => option<'b>) => t<'b>
let filterOk: t<result<'a, 'b>> => t<'a>
let filterSome: t<option<'a>> => t<'a>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
let flatten: t<t<'a>> => t<'a>
let indexed: t<'a> => t<('a, int)>
let intersperse: (t<'a>, 'a) => t<'a>
let intersperseWith: (t<'a>, unit => 'a) => t<'a>
let map: (t<'a>, 'a => 'b) => t<'b>
let mapi: (t<'a>, ('a, int) => 'b) => t<'b>
let pairwise: t<'a> => t<('a, 'a)>
let reverse: t<'a> => t<'a>
let scan: (t<'a>, 'b, ('b, 'a) => 'b) => t<'b>
let scani: (t<'a>, ~zero: 'b, (~sum: 'b, ~val: 'a, ~inx: int) => 'b) => t<'b>
let takeAtMost: (t<'a>, int) => t<'a>
let takeUntil: (t<'a>, 'a => bool) => t<'a>
let takeWhile: (t<'a>, 'a => bool) => t<'a>
let tap: (t<'a>, 'a => unit) => t<'a>
let window: (t<'a>, int) => t<array<'a>>
let windowAhead: (t<'a>, int) => t<array<'a>>
let windowBehind: (t<'a>, int) => t<array<'a>>

// Combine

let allPairs: (t<'a>, t<'b>) => t<('a, 'b)>
let concat: (t<'a>, t<'a>) => t<'a>
let interleave: (t<'a>, t<'a>) => t<'a>
let map2: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>
let map3: (t<'a>, t<'b>, t<'c>, ('a, 'b, 'c) => 'd) => t<'d>
let map4: (t<'a>, t<'b>, t<'c>, t<'d>, ('a, 'b, 'c, 'd) => 'e) => t<'e>
let map5: (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, ('a, 'b, 'c, 'd, 'e) => 'f) => t<'f>
let prepend: (t<'a>, t<'a>) => t<'a>
let sortedMerge: (t<'a>, t<'a>, ('a, 'a) => int) => t<'a>
let zip: (t<'a>, t<'b>) => t<('a, 'b)>
let zip3: (t<'a>, t<'b>, t<'c>) => t<('a, 'b, 'c)>
let zip4: (t<'a>, t<'b>, t<'c>, t<'d>) => t<('a, 'b, 'c, 'd)>
let zip5: (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>) => t<('a, 'b, 'c, 'd, 'e)>

// Reduce, consume, and calculate

let allOk: t<result<'a, 'b>> => result<t<'a>, 'b>
let allSome: t<option<'a>> => option<t<'a>>
let compare: (t<'a>, t<'b>, ('a, 'b) => int) => int
let consume: t<'a> => unit
let equals: (t<'a>, t<'b>, ('a, 'b) => bool) => bool
let everyOrEmpty: (t<'a>, 'a => bool) => bool
let exactlyOne: t<'a> => option<'a>
let find: (t<'a>, 'a => bool) => option<'a>
let findMap: (t<'a>, 'a => option<'b>) => option<'b>
let findMapi: (t<'a>, ('a, int) => option<'b>) => option<'b>
let forEach: (t<'a>, 'a => unit) => unit
let forEachi: (t<'a>, ('a, int) => unit) => unit
let head: t<'a> => option<'a>
let headTail: t<'a> => option<('a, t<'a>)>
let isEmpty: t<'a> => bool
let isSortedBy: (t<'a>, ('a, 'a) => int) => bool
let joinString: t<string> => string
let last: t<'a> => option<'a>
let length: t<'a> => int
let maxBy: (t<'a>, ('a, 'a) => int) => option<'a>
let minBy: (t<'a>, ('a, 'a) => int) => option<'a>
let orElse: (t<'a>, t<'a>) => t<'a>
let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b
let reducei: (t<'a>, ~zero: 'b, (~sum: 'b, ~val: 'a, ~inx: int) => 'b) => 'b
let some: (t<'a>, 'a => bool) => bool
let tail: t<'a> => t<'a>
let toArray: t<'a> => array<'a>
let toOption: t<'a> => option<t<'a>>
```
