# Rescript Extras

Useful general-purpose functions and modules for Rescript projects. Includes extensions to `Option`, `Result`, and `Array` as well as additional module `Cmp`, `CmpUtilities`, `Task`, and `Test`.

## Option

Existential quanitifiers **`isSomeAnd`** and **`isNoneOr`**.

Create an option from a function that may fail using **`fromTryCatch`**.

Combine two options with **`concat`**, where `None` functions like a zero.

"Add" an option to a regular value using **`fold`** and **`foldBack`**.

Convert **`toArray`**, **`fromOk`**, and **`fromError`**

## Array

Generate an array from a generator function using **`fromSeed`**, similar to `unfold` in other standard libraries.

Convert **`fromOption`**

In pipeline mode, easily create an array **`fromOneValue`**

## Result

Convert an array of results to a single result using **`fromArray`** and **`fromArrayMap`**.

Transform the error with **`mapError`**.

Create a result from a function that may fail with **`fromTryCatch`**.

## Task

Inspired by `fp-ts`, a `Task` is a lazy promise. Transform it before execution with **`map`** and **`mapError`**. When ready to use it, call **`toPromise`**.

## Cmp and CmpUtilities

The `Cmp.t` is the `('a,'a) => int` comparison function. Comparison utilities such as as **`eq`**, **`neq`**, **`lt`**, **`gte`**, **`min`**, **`max`**, etc. General a comparator **`fromMap`** or **`reverse`** the direction.

**Functors** to add sorting and equality functions to custom data types.

## Test runner

Super-simple test runner. Make tests using **`make`** and **`makeAsync`**. Run them using **`runSuite`**.
