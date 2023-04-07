# ReScript Extras

General-purpose modules for [ReScript](https://rescript-lang.org) projects. Includes extensions to `Option`, `Result`, and `Array`. Provides a lazy-promise `Task` and `TaskResult`, `NonEmptyArray`, comparison utilities in `Cmp`, and a very simple test runner `Test`. Includes experimental `Union` functors that can create, discriminate, and pattern match on untagged unions and a `Literal` module for support. To install:

1. `npm install @jmagaram/rescript-extras`
2. Add `@jmagaram/rescript-extras` to `bs-dependencies` in your `bsconfig.json`
3. In your code, `module E = Extras`

## Task and TaskResult

Inspired by [TaskEither in fp-ts](https://gcanti.github.io/fp-ts/modules/TaskEither.ts.html), a `Task` is a lazy promise that never fails. A `TaskResult` is a lazy promise that always returns an `Error` or an `Ok`. Just like a regular `result`, you can transform it before execution with **`map`**, **`mapError`**, and **`flatMap`**. When ready to execute, call **`toPromise`**.

## Union

ReScript uses tagged unions to discriminate between kinds in a variant. The **`Union`** module provides functors to create **untagged** unions of 2, 3, or 4 items. Capabilities:

- Discriminate on any programmable criteria, like `typeof`, `instance of`, lightweight shape detection (such as a tag), or full-blown validation with a JSON parsing library.
- Can be used for purely untagged unions, as well as unions tagged differently than how ReScript does it.
- Pattern matching
- Custom equality implementation
- Type safety

The disadvantages over compiler support for this feature are:

- Pattern matching must rely on a `match` function, not the usual matching syntax.
- Each case is distinguished a `A` | `B` | `C` | `D`, not terms the user defines. This can be fixed by extending or wrapping the module.
- Literal support and functors are a bit cumbersome

## Literal

Functors to create **`Literal`** types of various kinds using functors like **`MakeString`** and **`MakeInt`**. Includes built-in literals for `True`, `False`, `Null`, and `Undefined`. You create literals from reference types and provide a custom equality operator. Type safe; if you have a type that requires a "yes" you can't just pass any string in.

## Option

Existential quanitifiers **`isSomeAnd`** and **`isNoneOr`**. Create an option from a function that may fail using **`fromTryCatch`**. Combine two options with **`concat`** and **`map2`**. "Add" an option to a regular value using **`fold`** and **`foldBack`**. Lazy forms such as **`orElseWith`**.

## NonEmptyArray

An array that must have at least one item in it. All the usual functions like **`reduce`**, **`maxBy`**, **`minBy`**, **`map`**, **`mapi`**, **`concat`**, **`head`**, etc.

## Array

Generate an array from a generator function using **`fromSeed`**, similar to `unfold` in other standard libraries. Various utilities like **`pairwise`**, **`tail`**, **`head`**, **`tail`**, **`lastIndex`**, **`isEmpty`**, **`prepend`**, and **`fromOneValue`**

## Result

Convert an array of results to a single result using **`fromArray`** and **`fromArrayMap`**.

Create a result from a function that may fail with **`fromTryCatch`**.

Transform an error with **`mapError`**.

## Unknown

Similar to the `Types` module, includes functions to safely inspect unknown values like **`toString`** and **`toInt`** that returns an option. Can inspect properties on objects as well. Not intended for full-featured JSON parsing.

## Cmp and CmpUtilities

The `Cmp.t` is the `('a,'a) => int` comparison function. Comparison utilities such as as **`eq`**, **`neq`**, **`lt`**, **`gte`**, **`min`**, **`max`**, etc. General a comparator **`fromMap`** or **`reverse`** the direction.

Functors **`MakeCompare`** and **`MakeEquals`** add sorting and equality functions to your custom data types.

## Test runner

Super-simple test runner. Make tests using **`make`** and **`makeAsync`**. Run them using **`runSuite`**.

## Other

Various conversion functions like **`Option.toArray`**, **`Option.fromError`**, **`Array.fromOption`**, etc.
