# Contributing

If you want to add some functions or improve the package, start by posting an issue for discussion. All code needs **tests** and **documentation**.

## Documentation

- Include a code sample
- Look at other docs and try to make it fit in
- Code for samples should run through tests

## Tests

- For each new function, add a collection of tests
- Do one stress test to ensure no stack overflows, especially with recursion
- Think about how the behavior changes with dynamic/non-persistent sequences, such as where each item is generated on the fly like Js.Math.random(). For example, `allPairs` chooses to cache the values before generating the pairs.
- Ensure any supplied functions are only called the minimum number of times.
- Try taking 0 items from an infinite sequence as a test of complete laziness.

### Coding style

- Use `xx`, `yy`, `zz` for sequences
- Use `x`, `y`, `z` for items in those sequences
- Use `xxx` for a nested sequence
- Use `inx` for index variables
