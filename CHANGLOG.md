## Version 0.11.0

- More `Union` tests to make sure they work and demonstrate how to use them in practice. Include examples with convenience functions so you don't have to remember or pattern match on `A`, `B`, and `C`.
- Add convenience functions in `Union` to pattern match on a single case, like `toA` and `toB`.
- In `Union`, fix bug in basic int, string, float, etc. patterns that made them unusable because the types were abstract.
- In `Union`, fix bug where matching input could be anything but should have been just the union type.
