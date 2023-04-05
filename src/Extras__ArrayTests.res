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

let others = {
  let make = (~title, ~expect, ~a, ~b) =>
    T.make(~category="Array", ~title, ~expectation=expect, ~predicate=() => a() == b)
  [
    make(~title="last", ~expect="when empty, return None", ~a=() => []->A.last, ~b=None),
    make(
      ~title="last",
      ~expect="when not empty, return Some",
      ~a=() => [1, 2, 3]->A.last,
      ~b=Some(3),
    ),
    make(
      ~title="tail",
      ~expect="when not empty, return rest",
      ~a=() => [1, 2]->A.tail,
      ~b=Some([2]),
    ),
    make(
      ~title="tail",
      ~expect="when not empty, return rest",
      ~a=() => [1, 2, 3]->A.tail,
      ~b=Some([2, 3]),
    ),
    make(~title="tail", ~expect="when single item, return None", ~a=() => [1]->A.tail, ~b=None),
    make(~title="head", ~expect="when empty, return None", ~a=() => []->A.head, ~b=None),
    make(~title="head", ~expect="when not empty, return first", ~a=() => [1]->A.head, ~b=Some(1)),
    make(
      ~title="lastIndex",
      ~expect="when more than 1 item, return Some",
      ~a=() => [1, 2, 3]->A.lastIndex,
      ~b=Some(2),
    ),
    make(
      ~title="lastIndex",
      ~expect="when one item, return Some",
      ~a=() => [1]->A.lastIndex,
      ~b=Some(0),
    ),
    make(~title="lastIndex", ~expect="when empty, return None", ~a=() => []->A.lastIndex, ~b=None),
    make(~title="pairwise", ~expect="when empty, return empty", ~a=() => []->A.pairwise, ~b=[]),
    make(~title="pairwise", ~expect="when one item, return empty", ~a=() => [1]->A.pairwise, ~b=[]),
    make(
      ~title="pairwise",
      ~expect="when two items, return one pair",
      ~a=() => [1, 2]->A.pairwise,
      ~b=[(1, 2)],
    ),
    make(
      ~title="pairwise",
      ~expect="when many items, return all pairs",
      ~a=() => [1, 2, 3, 4]->A.pairwise,
      ~b=[(1, 2), (2, 3), (3, 4)],
    ),
  ]
}

let tests = [others, fromSeed, fromOption, fromOneValue]->Belt.Array.concatMany
