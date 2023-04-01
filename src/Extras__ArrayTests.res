module T = Extras__Test
module A = Extras__Array

let fromSeed = {
  let make = (expectation, seed, generator, expected) =>
    T.make(~category="Array", ~title="fromSeed", ~expectation, ~predicate=() =>
      seed->A.fromSeed(generator) == expected
    )
  [
    ("multiplicative", 1, i => i < 100 ? Some(i, i * 2) : None, [1, 2, 4, 8, 16, 32, 64]),
    ("multiplicative", 28, i => i < 100 ? Some(i, i * 2) : None, [28, 56]),
    ("multiplicative", 101, i => i < 100 ? Some(i, i * 2) : None, []),
    ("simple range up", 5, i => i <= 20 ? Some(i, i + 5) : None, [5, 10, 15, 20]),
    ("simple range down", 20, i => i > 15 ? Some(i, i - 1) : None, [20, 19, 18, 17, 16]),
  ]->Belt.Array.map(((expectation, seed, f, goal)) => make(expectation, seed, f, goal))
}

let fromOneValue = {
  let make = (expectation, predicate) =>
    T.make(~category="Array", ~title="fromOneValue", ~expectation, ~predicate)
  [
    ("if primitive => array of primitive", () => 3->A.fromOneValue == [3]),
    ("if array => array of array", () => [1, 2, 3]->A.fromOneValue == [[1, 2, 3]]),
  ]->Belt.Array.map(((expectation, predicate)) => make(expectation, predicate))
}

let fromOption = {
  let make = (expectation, predicate) =>
    T.make(~category="Array", ~title="fromOption", ~expectation, ~predicate)
  [
    ("if None => empty", () => None->A.fromOption == []),
    ("if Some => array of one value", () => Some(3)->A.fromOption == [3]),
  ]->Belt.Array.map(((expectation, predicate)) => make(expectation, predicate))
}

let tests = [fromSeed, fromOption, fromOneValue]->Belt.Array.concatMany
