# ReScript Extras

Utilities for [ReScript](https://rescript-lang.org). Includes:

- Lazy Sequences with a `Seq` module; [see README](SEQ_README.md) for overview, functions, samples, etc.
- A lazy-promise `Task` and `TaskResult`
- Untagged `Union` and `Literal` to construct and pattern match on discriminated unions of any kind
- Useful extensions to `Option`, `Result`, and `Array`
- `NonEmptyArray`
- Comparison utilities
- `Trampoline` module to remove recursion
- A simple `Test` runner

## To install

```sh
npm install @jmagaram/rescript-extras
```

Add to your `bsconfig.json`...

```diff
{
  ...
+ "bs-dependencies": ["@jmagaram/rescript-extras"]
+ "bsc-flags": ["-open @jmagaram/rescript-extras"],
}
```

## Lazy sequences (Seq module)

See [Seq README](SEQ_README.md)

## Task and TaskResult

Inspired by [TaskEither in fp-ts](https://gcanti.github.io/fp-ts/modules/TaskEither.ts.html), a `Task` is a lazy promise that never fails. The `Task.Result` works with lazy promises that always return an `Error` or an `Ok`. Just like a regular `result`, you can transform it before execution with **`map`**, **`mapError`**, **`flatMap`**, and **`toOption`**. When ready to execute, call **`toPromise`**.

## Union

The **`Union`** module provides functors to create tagged and untagged discriminated unions of 2, 3, 4, or 5 items. This is useful for interop with JavaScript libraries that produce or expect simple types like `string | number` and more complex types where the choices are tagged differently than how ReScript does it. See [usage examples](tests/Extras__UnionTests.res). Capabilities:

- Discriminate (pattern match) on any programmable criteria, like `typeof`, `instance of`, lightweight shape detection (such as a tag), or parsing with a JSON parsing library.
- All types can participate in a union.
- Custom equality
- Literal values like `Null`, `Undefined`, `True`, `False`, `-1`, and custom literals via `MakeString`, `MakeInt`, etc.
- Built-in support for `Int`, `String`, `Bool`, `Float`, `option<t>`, `nullable<t>`, `null<t>`, and tuples of length 2 and 3

This implementation does not utilize any special compiler support and so there are some limitations:

- Pattern matching must rely on a `match` function, not the usual matching syntax. Each case is distinguished by `A` | `B` | `C` | `D`. This can be easily improved by extending or wrapping the produced module; see the [test file for examples](tests/Extras__UnionTests.res).
- Literal support and functors are a bit cumbersome
- No genType support
- No recursive type definition

**Note:** It is possible to create untagged unions with a library like
[rescript-struct](https://github.com/DZakh/rescript-struct) and not use the functors defined in this package. See the [usage examples](tests/Extras__UnionTests.res) for a comparision of the two approaches. This works very well and avoids abstractions, but requires a bit more code.

**Note:** The Rescript compiler has a new feature for untagged unions that works great with pattern matching support. There are limitations on which types can be included in the union, literal support is very nice, pattern matching is not customizable, and custom equality is not provided.

## Literal

Functors to create **`Literal`** types of various kinds using **`MakeString`** and **`MakeInt`** and others. Includes built-in literals for `True`, `False`, `Null`, and `Undefined`. You can create literals from reference types, and provide a custom equality operator to do things like case-insensitive string comparison. This implementation is completely type safe because each literal is its own unique type; you can't just cast any string to a "yes" for example.

## Option

Existential quanitifiers **`isSomeAnd`** and **`isNoneOr`**. Create an option from a function that may fail using **`fromTryCatch`**. Combine options with **`concat`**, **`map2`**, **`map3`**, and **`map4`**. "Add" an option to a regular value using **`fold`** and **`foldBack`**. Includes lazy forms such as **`orElseWith`**.

## NonEmptyArray

An array that must have at least one item in it. Include many of the usual functions like **`reduce`**, **`maxBy`**, **`minBy`**, **`map`**, **`mapi`**, **`concat`**, **`head`**, etc. Convert **`toArray`** and **`fromArray`**.

## Array

Generate an array from a generator function using **`fromSeed`**, similar to `unfold` in other standard libraries. Includes various utilities like **`pairwise`**, **`tail`**, **`head`**, **`tail`**, **`lastIndex`**, **`isEmpty`**, **`prepend`**, and **`fromOneValue`**

## Result

Convert an array of results to a single result using **`fromArray`** and **`fromArrayMap`**. Create a result from a function that may fail with **`fromTryCatch`**. Transform an error with **`mapError`**.

## Unknown

Similar to the `Types` module, includes functions to safely inspect unknown values like **`toString`** and **`toInt`** that returns an option. Can inspect properties on objects as well. Not intended for full-featured JSON parsing.

## Cmp and CmpUtilities

`Cmp.t` is the `('a,'a) => int` comparison function. The **`Cmp`** module provides comparison utilities such as as **`eq`**, **`neq`**, **`lt`**, **`gte`**, **`min`**, and **`max`**. **`fromMap`** makes it easy to generate a comparison function for an object based on a specific property in it. Or use **`reverse`** to change direction.

Functors **`MakeCompare`** and **`MakeEquals`** add sorting and equality functions to your custom data types.

## Trampoline

Simple tools to remove recursion and avoid stack overflows.

## Test runner

Super-simple test runner. Make tests using **`make`** and **`makeAsync`**. Run them using **`runSuite`**.
