## Version 2.0.0

- Fixed
  - `Seq.filterMap` didn't work with nested options
  - `Seq.tail` was not completely lazy
  - `Seq.dropAtMost` was not completely lazy
  - `Seq.findMap` didn't work with nested options
  - `Seq.windowAhead` would hang if window size was enormous
  - `Seq.replicate` and `Seq.replicateWith` argument order was not data-first
- Add
  - `Seq.reduceUntil` to short-circuit a reduce operation
  - `Seq.reduceWhile` to short-circuit a reduce operation
  - `Seq.chunkBy` to reduce adjacent items
  - `Seq.chunkByKey` to reduce adjacent (key, value) pairs by key
  - `Seq.sumBy` (like `Seq.reduce` but no initial parameter)
  - `Seq.prefixSum` (like `scan` but does not take an initial parameter)
  - `Seq.toList`
  - `Seq.tails`
  - `Seq.dropLast`
  - `NonEmptyArray.of1`, `of2`, `of3` and `ofMany`, all zero-cost bindings to `Array.of`.
  - In the test framework, enable displaying detailed messages when a test fails. Provide `Test.fromResult` and `Test.fromPredicate`. Also `expectThrow` and `expectsNotThrow`. The expectation text is optional.
- Rename
  - `repeat` to `replicate`; more like other Seq libraries
  - `InvalidArgument` exception not `ArgumentOfOfRange` (spelled wrong)
  - `Seq.every` not `Seq.everyOrEmpty`; less cumbersome and more like array
  - `Seq.everyOk` not `Seq.allSome` (using `every` not `forAll`)
  - `Seq.everySome` not `Seq.allSome` (using `every` not `forAll`)
  - `Seq.join` like `Array.join` rather than `Seq.joinString`. Also require a separator character.
  - `Seq.headTail` to `Seq.uncons`; less cumbersome and used in other libraries.
  - `Seq.once` not `Seq.singleton`; like Rust. Also provide `Seq.onceWith`
  - `Array.exactlyOne` not `Array.exactlyOneValue` like `Seq.exactlyOne`
  - `Array.of1` not `Array.fromOneValue`
- Remove
  - `Array.fromSeed`; use Seq module instead
  - `Array.pairs`; use Seq module instead
  - `Array.filterSomeWith`; use Seq module instead
  - `Seq.characters`; many ways to split a string and with Core it will feel better since you can do `String.split` not `Js.String2.split`.
  - `Seq.startWith` and `Seq.endWith`; use `concat` and `prepend` with `Seq.once`.
  - `Seq.scani`; just call `indexed` beforehand if index is needed. The mapping within scan removes the index if it isn't desired, unlike `filteri`.
  - `Seq.reducei`; just call `indexed` beforehand if index is needed. The mapping within reduce removes the index if it isn't desired, unlike `filteri`.
- Other
  - Cleanup lots of code

## Version 1.1.1

- Fix bug in `Seq.takeAtMost` where generator function was called 1 too many times; not lazy enough
- `Seq.combinations`
- `Seq.permutations`

## Version 1.1.0

- Fix bug in `allPairs` where sequences are not cached.
- `Seq.reverse`
- `Seq.sortBy`
- `Seq.delay`

## Version 1.0.0

- Add `Seq` module

## Version 0.22.0

- `Trampoline` module to eliminate recursion in functions
- `Option.map5`
- Lazy sequences in `Seq` module; will split into separate repository

## Version 0.21.0

- Make `Task.t` not abstract
- `Option.map2`, `Option.map3`, `Option.map4` and test cleanup
- `Option.flatten`
- `Array.filterSome` and `Array.filterSomeWith`
- For untagged unions
  - `Pattern.MakeOption` - Turn any pattern into a `t | undefined`
  - `Pattern.MakeNullable` - Turn any pattern into a `t | undefined | null`
  - `Pattern.MakeNull` - Turn any pattern into a `t | null`
  - `Pattern.MakeTuple2` and `Pattern.MakeTuple3`
- Option to only show test failures

## Version 0.20.0

- Use the built-in `unknown` type
- Move `Pattern` to a top-level module, not inside the `Union` module.
- Lazy parsing in unions; only do enough until find a match

## Version 0.19.0

- Move tests into separate folder and not included in package
- Only includes files needed in package

## Version 0.14.0

- Move `TaskResult` into a submodule of `Task` for usability

## Version 0.13.0

- Switch to `commonjs` module format; supposedly this works better for Rescript libraries
- Examples of using [rescript-struct](https://github.com/DZakh/rescript-struct) for unions, and compared to using functors in this package.
- Add `TaskResult.mapBoth`
- Add `Task.spy`
- Fix genType bug with `TaskResult.t`

## Version 0.12.0

### Union

- Rename `match` to `matchABC` so if a convenience function is written to wrap the `onA`, `onB`, etc. it can use the friendlier `match` word.
- More tests to make sure they work and demonstrate how to use them in practice. Include examples with convenience functions so you don't have to remember or pattern match on `A`, `B`, and `C`.
- Add convenience functions to pattern match on a single case, like `toA` and `toB`.
- Fix bug in basic int, string, float, etc. patterns that made them unusable because the types were abstract.
- Fix bug where matching input could be anything but should have been just the union type.

### Other

- Fix bug in `Unknown.isNullOrUndefined`; only checked for undefined
