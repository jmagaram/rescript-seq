## Version 2.0.0

- Remove `Array.fromSeed`; use Seq module instead
- Remove `Array.pairs`; use Seq module instead
- Remove `Array.filterSomeWith`
- Rename exception from the spelled-wrong `ArgumentOfOfRange` to `InvalidArgument`
- Rename `Array.exactlyOneValue` to `Array.exactlyOne` like `Seq.exactlyOne`
- Rename `Seq.singleton` to `Seq.once` and add `Seq.onceWith`; like Rust does it
- Rename `Seq.everyOrEmpty` to `Seq.every` to be more like array; less cumbersome
- Rename `Seq.allSome` and `Seq.allOk` to `Seq.everySome` and `Seq.everyOk`
- Remove `Seq.characters`; many ways to split a string and with Core it will feel better since you can do `String.split` not `Js.String2.split`.
- Rename `Seq.joinString` to `Seq.join` like `Array.join`. Require a separator character.
- Rename `Seq.takeAtMost` to `Seq.take` since that is the TC39 proposal name and other packages use the same name. The name is slightly inaccurate, but then so is `drop`.
- Add `Seq.toList`
- Replace `Array.fromOneValue` with `Array.of1`
- Replace `NonEmptyArray.fromOneValue` with `of1`, `of2`, `of3` and `ofMany`, all zero-cost bindings to `Array.of`.
- `Test.fromResult` and `Test.fromPredicate` and async flavors; enable displaying message when test fails. Also show detailed of test failures.

## Version 1.1.1

- Fix bug in `Seq.take` where generator function was called 1 too many times; not lazy enough
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
