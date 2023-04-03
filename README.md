# ReScript Extras

General-purpose modules for [ReScript](https://rescript-lang.org) projects. Includes extensions to `Option`, `Result`, and `Array`. Provides a lazy-promise `Task` and `TaskResult`, comparison utilities in `Cmp`, and a very simple test runner `Test`. To install:

1. `npm install @jmagaram/rescript-extras`
2. Add `@jmagaram/rescript-extras` to `bs-dependencies` in your `bsconfig.json`
3. In your code, `module E = Extras`

## Task and TaskResult

Inspired by [TaskEither in fp-ts](https://gcanti.github.io/fp-ts/modules/TaskEither.ts.html), a `Task` is a lazy promise that never fails. A `TaskResult` is a lazy promise that always returns an `Error` or an `Ok`. Just like a regular `result`, you can transform it before execution with **`map`**, **`mapError`**, and **`flatMap`**. When ready to execute, call **`toPromise`**.

## Option

Existential quanitifiers **`isSomeAnd`** and **`isNoneOr`**.

Create an option from a function that may fail using **`fromTryCatch`**.

Combine two options with **`concat`** and **`map2`**.

"Add" an option to a regular value using **`fold`** and **`foldBack`**.

## Array

Generate an array from a generator function using **`fromSeed`**, similar to `unfold` in other standard libraries.

In pipeline mode, easily create an array **`fromOneValue`**

## Result

Convert an array of results to a single result using **`fromArray`** and **`fromArrayMap`**.

Create a result from a function that may fail with **`fromTryCatch`**.

Transform an error with **`mapError`**.

## Cmp and CmpUtilities

The `Cmp.t` is the `('a,'a) => int` comparison function. Comparison utilities such as as **`eq`**, **`neq`**, **`lt`**, **`gte`**, **`min`**, **`max`**, etc. General a comparator **`fromMap`** or **`reverse`** the direction.

Functors **`MakeCompare`** and **`MakeEquals`** add sorting and equality functions to your custom data types.

## Test runner

Super-simple test runner. Make tests using **`make`** and **`makeAsync`**. Run them using **`runSuite`**.

## Other

Various conversion functions like **`Option.toArray`**, **`Option.fromError`**, **`Array.fromOption`**, etc.
