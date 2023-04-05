module NEA = Extras__NonEmptyArray
module O = Belt.Option

let expectEq = (~title, ~expectation, ~a, ~b) =>
  Extras__Test.make(~category="NonEmptyArray", ~title, ~expectation, ~predicate=() => a() == b)

let one = [1]->NEA.fromArrayExn
let oneTwo = [1, 2]->NEA.fromArrayExn
let oneTwoThree = [1, 2, 3]->NEA.fromArrayExn
let add = (x, y) => x + y

let tests = [
  expectEq(
    ~title="fromArray",
    ~expectation="when empty => None",
    ~a=() => []->NEA.fromArray,
    ~b=None,
  ),
  expectEq(
    ~title="fromArray",
    ~expectation="when one item => Some",
    ~a=() => [1]->NEA.fromArray->O.map(NEA.toArray),
    ~b=Some([1]),
  ),
  expectEq(
    ~title="fromArray",
    ~expectation="when many items => Some",
    ~a=() => [1, 2, 3]->NEA.fromArray->O.map(NEA.toArray),
    ~b=Some([1, 2, 3]),
  ),
  expectEq(
    ~title="map",
    ~expectation="",
    ~a=() => oneTwoThree->NEA.map(i => i * 2)->NEA.toArray,
    ~b=[2, 4, 6],
  ),
  expectEq(~title="head", ~expectation="returns first item", ~a=() => oneTwoThree->NEA.head, ~b=1),
  expectEq(~title="last", ~expectation="returns last item", ~a=() => oneTwoThree->NEA.last, ~b=3),
  expectEq(
    ~title="reduce",
    ~expectation="when one item, return it",
    ~a=() => one->NEA.reduce(add),
    ~b=1,
  ),
  expectEq(
    ~title="reduce",
    ~expectation="when two items, return sum",
    ~a=() => oneTwo->NEA.reduce(add),
    ~b=3,
  ),
  expectEq(
    ~title="reduce",
    ~expectation="when many items, return sum",
    ~a=() => oneTwoThree->NEA.reduce(add),
    ~b=6,
  ),
  expectEq(
    ~title="maxBy",
    ~expectation="return max",
    ~a=() => oneTwoThree->NEA.maxBy((i, j) => i < j ? -1 : i > j ? 1 : 0),
    ~b=3,
  ),
  expectEq(
    ~title="minBy",
    ~expectation="return min",
    ~a=() => oneTwoThree->NEA.minBy((i, j) => i < j ? -1 : i > j ? 1 : 0),
    ~b=1,
  ),
  expectEq(
    ~title="concat",
    ~expectation="",
    ~a=() => oneTwoThree->NEA.concat(oneTwoThree)->NEA.toArray,
    ~b=[1, 2, 3, 1, 2, 3],
  ),
  expectEq(
    ~title="fromOneValue",
    ~expectation="",
    ~a=() => 4->NEA.fromOneValue->NEA.toArray,
    ~b=[4],
  ),
]
