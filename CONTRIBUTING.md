# Contributing

If you want to add some functions or improve the package, start by posting an issue for discussion. All code needs **tests** and **documentation**. Look at other sequence and list packages to see how they solve a similar problem. Explain the use case.

## Documentation

- Look at other docs and try to make it fit in
- Consider a code sample

## Tests

- For each new function, add a collection of tests
- Do one stress test to ensure no stack overflows, especially with recursion
- Think about how the behavior changes with dynamic/non-persistent sequences, such as where each item is generated on the fly like Js.Math.random(). For example, `allPairs` chooses to cache the values before generating the pairs.
- Is there internal mutable state? How will that break things?
- Ensure any supplied functions are only called the minimum number of times; use the `death` sequence to ensure your function is as lazy as possible.

### Coding style

- Try to code each function from scratch, rather than having a lot of abstraction.
- Avoid labeled arguments unless really confusing
- Throw `InvalidArgument` exception if the parameters are clearly being misused.
- Use `xx`, `yy`, `zz` for sequences
- Use `x`, `y`, `z` for items in those sequences
- Use `xxx` for a nested sequence
- Use `inx` for index variables
