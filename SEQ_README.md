# Lazy Sequences for ReScript

A _sequence_ is a list whose elements are computed only on demand. Sequences are produced and transformed lazily (one element at a time) rather than eagerly (all at once). This allows constructing conceptually infinite sequences. A sequence can provide better performance than an `array` when not all elements are used. Sequences are similar to [JavaScript iterables](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols).

Sequences are an important and convenient data structure in [F# (Seq module)](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-seqmodule.html) , [OCaml (Seq module)](https://v2.ocaml.org/api/Seq.html), [Rust std::iter](https://doc.rust-lang.org/std/iter/trait.Iterator.html), [`C# IEnumerable<T>`](https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.ienumerable-1?view=net-8.0), and [Python (itertools)](https://docs.python.org/3/library/itertools.html). There are many libraries that help consume iterables in JavaScript as well.

:boom: **This is a comprehensive library for creating and consuming sequences in ReScript.**:boom:

Highlights:

- Enables more elegant and concise solutions to problems than using imperative code and arrays
- All APIs documented; some with code examples
- Comprehensive test suite
- Written 100% in ReScript; look at the tests and code to gain deeper understanding of how it works
- Full suite of functions based on researching the other libraries and languages
- Ready for your contributions! :two_men_holding_hands:
- Easily **construct sequences** using `fromArray`, `range`, `unfold`, `cycle`, `repeat` and others.
- **Transform** sequences with `map`, `filter`, `takeAtMost`, `dropWhile`, `scan`, `tap`, `window`, and others.
- **Combine** multiple sequences with `zip`, `zip3`, `map2`, `map3`, `sortedMerge`, `interleave` and others.
- **Calculate** values with `reduce`, `some`, `findMap`, `isSortedBy`, `minBy`, `toArray` and others.

See the [examples](#examples) section.

## Install

```sh
npm install @jmagaram/rescript-seq
```

Add `@jmagaram/rescript-seq` to `bs-dependencies` in your `bsconfig.json`:

```diff
{
  ...
+ "bs-dependencies": ["@jmagaram/rescript-seq"]
+ "bsc-flags": ["-open @jmagaram/rescript-seq"],
}
```

## Functions

```rescript
type t<'a> // A lazy sequence of `a`

// Construct

let characters: Js.String2.t => t<Js.String2.t>
let cons: ('a, t<'a>) => t<'a>
let cycle: t<'a> => t<'a>
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
let chunkBySize: (t<'a>, int) => t<Js.Array2.t<'a>>
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
let scan: (t<'a>, 'b, ('b, 'a) => 'b) => t<'b>
let scani: (t<'a>, ~zero: 'b, (~sum: 'b, ~val: 'a, ~inx: int) => 'b) => t<'b>
let takeAtMost: (t<'a>, int) => t<'a>
let takeUntil: (t<'a>, 'a => bool) => t<'a>
let takeWhile: (t<'a>, 'a => bool) => t<'a>
let tap: (t<'a>, 'a => unit) => t<'a>
let window: (t<'a>, int) => t<Js.Array2.t<'a>>
let windowAhead: (t<'a>, int) => t<Js.Array2.t<'a>>
let windowBehind: (t<'a>, int) => t<Js.Array2.t<'a>>

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
let headTails: t<'a> => t<('a, t<'a>)>
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
let toArray: t<'a> => Js.Array2.t<'a>
let toOption: t<'a> => option<t<'a>>
```

## Usage examples

```rescript
let intToString = Belt.Int.toString
type point = {x: int, y: int}
let localMinimums = points =>
  points
  ->Seq.fromArray
  ->Seq.window(3)
  ->Seq.filterMap(pp => pp[1].y < pp[0].y && pp[1].y < pp[2].y ? Some(pp[1]) : None)
  ->Seq.map(p => `(${p.x->intToString},${p.y->intToString})`)
  ->Seq.intersperse(", ")
  ->Seq.joinString

let validateDocs = (documents, validate) =>
  switch documents
  ->Seq.fromArray
  ->Seq.filter(doc => doc["status"] == "unprocessed")
  ->Seq.map(validate)
  ->Seq.allOk {
  | Ok(docs) => docs->Seq.map(doc => doc["title"])->Seq.forEach(Js.log)
  | Error(err) => Js.log(`First error: ${err}`)
  }

let numbers =
  Seq.infinite(() => Js.Math.random())
  ->Seq.filter(i => i < 0.3)
  ->Seq.pairwise
  ->Seq.filter(((a, b)) => a < b)
  ->Seq.takeAtMost(1000)
  ->Seq.toArray

let fibs = count =>
  Seq.unfold((0, 1), ((a, b)) => a + b <= 100 ? Some(a + b, (b, a + b)) : None)
  ->Seq.prepend([0, 1]->Seq.fromArray)
  ->Seq.takeAtMost(count)
  ->Seq.map(Belt.Int.toString)
  ->Seq.intersperse(", ")
  ->Seq.joinString
```
