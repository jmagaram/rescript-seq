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

let test = (~title, ~expect, ~a, ~b) =>
  T.make(~category="Array", ~title, ~expectation=expect, ~predicate=() => {
    let m = a()
    let n = b
    if m != n {
      Js.Console.log("")
      Js.Console.log(`== NOT EQUAL : ${title} ==`)
      Js.Console.log2("a: ", m)
      Js.Console.log2("b: ", n)
    }
    m == n
  })

let others = {
  [
    test(
      ~title="fromOneValue",
      ~expect="wrap item in an array",
      ~a=() => [1, 2, 3]->A.fromOneValue,
      ~b=[[1, 2, 3]],
    ),
    test(~title="fromOption", ~expect="if None => empty", ~a=() => None->A.fromOption, ~b=[]),
    test(
      ~title="fromOption",
      ~expect="if Some => array with one item",
      ~a=() => Some(3)->A.fromOption,
      ~b=[3],
    ),
    test(~title="last", ~expect="when empty, return None", ~a=() => []->A.last, ~b=None),
    test(
      ~title="last",
      ~expect="when not empty, return Some",
      ~a=() => [1, 2, 3]->A.last,
      ~b=Some(3),
    ),
    test(~title="head", ~expect="when empty, return None", ~a=() => []->A.head, ~b=None),
    test(~title="head", ~expect="when not empty, return first", ~a=() => [1]->A.head, ~b=Some(1)),
    test(
      ~title="lastIndex",
      ~expect="when more than 1 item, return Some",
      ~a=() => [1, 2, 3]->A.lastIndex,
      ~b=Some(2),
    ),
    test(
      ~title="lastIndex",
      ~expect="when one item, return Some",
      ~a=() => [1]->A.lastIndex,
      ~b=Some(0),
    ),
    test(~title="lastIndex", ~expect="when empty, return None", ~a=() => []->A.lastIndex, ~b=None),
    test(~title="pairs", ~expect="when empty, return empty", ~a=() => []->A.pairs, ~b=[]),
    test(~title="pairs", ~expect="when one item, return empty", ~a=() => [1]->A.pairs, ~b=[]),
    test(
      ~title="pairs",
      ~expect="when two items, return one pair",
      ~a=() => [1, 2]->A.pairs,
      ~b=[(1, 2)],
    ),
    test(
      ~title="pairs",
      ~expect="when many items, return all pairs",
      ~a=() => [1, 2, 3, 4]->A.pairs,
      ~b=[(1, 2), (2, 3), (3, 4)],
    ),
    test(
      ~title="exactlyOneValue",
      ~expect="when empty => None",
      ~a=() => []->A.exactlyOneValue,
      ~b=None,
    ),
    test(
      ~title="exactlyOneValue",
      ~expect="when one value => Some",
      ~a=() => [3]->A.exactlyOneValue,
      ~b=Some(3),
    ),
    test(
      ~title="exactlyOneValue",
      ~expect="when empty => None",
      ~a=() => [1, 2]->A.exactlyOneValue,
      ~b=None,
    ),
    test(
      ~title="prepend",
      ~expect="concats to the beginning",
      ~a=() => [3, 4, 5]->A.prepend([1, 2]),
      ~b=[1, 2, 3, 4, 5],
    ),
    test(~title="filterSome", ~expect="when empty => []", ~a=() => []->A.filterSome, ~b=[]),
    test(
      ~title="filterSome",
      ~expect="when one Some(x) => [x]",
      ~a=() => [Some(3)]->A.filterSome,
      ~b=[3],
    ),
    test(~title="filterSome", ~expect="when one None => []", ~a=() => [None]->A.filterSome, ~b=[]),
    test(
      ~title="filterSome",
      ~expect="keep just the Some values",
      ~a=() => [None, Some(1), Some(2), None]->A.filterSome,
      ~b=[1, 2],
    ),
    test(
      ~title="filterSomeWith",
      ~expect="can filter by index",
      ~a=() => ["a", "b", "c"]->A.filterSomeWith((_, inx) => inx === 1 ? Some("bb") : None),
      ~b=["bb"],
    ),
    test(
      ~title="filterSomeWith",
      ~expect="can filter by value",
      ~a=() => ["a", "b", "c"]->A.filterSomeWith((value, _) => value === "b" ? Some("bb") : None),
      ~b=["bb"],
    ),
    test(
      ~title="indexed",
      ~expect="make tuples of item and the associated index",
      ~a=() => ["a", "b", "c"]->A.indexed,
      ~b=[("a", 0), ("b", 1), ("c", 2)],
    ),
  ]
}

let tests = [others, fromSeed]->Belt.Array.concatMany
