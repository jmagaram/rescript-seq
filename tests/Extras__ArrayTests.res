module T = Extras__Test
module A = Extras__Array

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

let concatSort = (xxs: array<array<'a>>, stringify: 'a => string) =>
  xxs
  ->Js.Array2.map(xs => xs->Js.Array2.map(stringify)->Js.Array2.joinWith(""))
  ->Js.Array2.sortInPlace

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
  ]
}

let tests = [others]->Belt.Array.concatMany
