module T = Extras__Test
module S = Extras__Seq

let areEqual = (~title, ~expectation, ~a, ~b) =>
  T.make(~category="Seq", ~title, ~expectation, ~predicate=() => {
    let a = a()->S.toArray
    if a != b {
      Js.Console.log(`===== NOT EQUAL : ${title} =====`)
      Js.Console.log(`A: ${a->Js.Array2.toString}`)
      Js.Console.log(`B: ${b->Js.Array2.toString}`)
    }
    a == b
  })

let constructors = [
  areEqual(~title="singleton", ~expectation="has one item in it", ~a=() => S.singleton(3), ~b=[3]),
  areEqual(~title="empty", ~expectation="has no items", ~a=() => S.empty, ~b=[]),
  areEqual(
    ~title="unfold",
    ~expectation="when many items",
    ~a=() => S.unfold(1, x => x <= 5 ? Some(x, x + 1) : None),
    ~b=[1, 2, 3, 4, 5],
  ),
  areEqual(
    ~title="unfold",
    ~expectation="when empty",
    ~a=() => S.unfold(1, x => x <= 5 ? Some(x, x + 1) : None),
    ~b=[1, 2, 3, 4, 5],
  ),
  areEqual(
    ~title="init",
    ~expectation="when count is < 0 => empty",
    ~a=() => S.init(~count=-1, ~initializer=(~index) => index->Belt.Int.toString),
    ~b=[],
  ),
  areEqual(
    ~title="init",
    ~expectation="when count is 0 => empty",
    ~a=() => S.init(~count=0, ~initializer=(~index) => index->Belt.Int.toString),
    ~b=[],
  ),
  areEqual(
    ~title="init",
    ~expectation="when count is 1 => singleton",
    ~a=() => S.init(~count=1, ~initializer=(~index) => index->Belt.Int.toString),
    ~b=["0"],
  ),
  areEqual(
    ~title="init",
    ~expectation="when count > 2 => map each index",
    ~a=() => S.init(~count=3, ~initializer=(~index) => index->Belt.Int.toString),
    ~b=["0", "1", "2"],
  ),
  areEqual(
    ~title="replicate",
    ~expectation="when count = 0 => empty",
    ~a=() => S.replicate(~count=0, ~value="x"),
    ~b=[],
  ),
  areEqual(
    ~title="replicate",
    ~expectation="when count = 1 => the item as singleton",
    ~a=() => S.replicate(~count=1, ~value="x"),
    ~b=["x"],
  ),
  areEqual(
    ~title="replicate",
    ~expectation="when count > 2 => the item repeated",
    ~a=() => S.replicate(~count=3, ~value="x"),
    ~b=["x", "x", "x"],
  ),
]

let oneTwoThree = S.init(~count=3, ~initializer=(~index) => index + 1)

let transforming = [
  areEqual(
    ~title="append",
    ~expectation="when both not empty",
    ~a=() => S.append(oneTwoThree, oneTwoThree),
    ~b=[1, 2, 3, 1, 2, 3],
  ),
  areEqual(
    ~title="append",
    ~expectation="when both empty",
    ~a=() => S.append(S.empty, S.empty),
    ~b=[],
  ),
  areEqual(
    ~title="append",
    ~expectation="when first empty",
    ~a=() => S.append(S.empty, oneTwoThree),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="append",
    ~expectation="when second empty",
    ~a=() => S.append(oneTwoThree, S.empty),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="flatMap",
    ~expectation="when map to several items => flatten",
    ~a=() => oneTwoThree->S.flatMap(i => S.replicate(~count=3, ~value=i)),
    ~b=[1, 1, 1, 2, 2, 2, 3, 3, 3],
  ),
  areEqual(
    ~title="flatMap",
    ~expectation="when original is empty => empty",
    ~a=() => S.empty->S.flatMap(_ => S.replicate(~count=5, ~value="x")),
    ~b=[],
  ),
  areEqual(
    ~title="flatMap",
    ~expectation="when original is one item",
    ~a=() => S.singleton(1)->S.flatMap(i => S.replicate(~count=5, ~value=i)),
    ~b=[1, 1, 1, 1, 1],
  ),
  areEqual(
    ~title="flatMap",
    ~expectation="when mapped result is empty => empty",
    ~a=() => oneTwoThree->S.flatMap(_ => S.empty),
    ~b=[],
  ),
  areEqual(
    ~title="flatMap",
    ~expectation="when mapped result is one item",
    ~a=() => oneTwoThree->S.flatMap(i => S.singleton(i->Belt.Int.toString)),
    ~b=["1", "2", "3"],
  ),
]

let consuming = [
  T.make(~category="Seq", ~title="toList", ~expectation="when empty", ~predicate=() =>
    S.empty->S.toReversedList == list{}
  ),
  T.make(~category="Seq", ~title="toList", ~expectation="when one item", ~predicate=() =>
    S.singleton(3)->S.toReversedList == list{3}
  ),
  T.make(
    ~category="Seq",
    ~title="toList",
    ~expectation="when several items, is reversed",
    ~predicate=() =>
      S.unfold(1, i => i <= 5 ? Some(i, i + 1) : None)->S.toReversedList == list{5, 4, 3, 2, 1},
  ),
]

let tests = [constructors, transforming, consuming]->Belt.Array.flatMap(i => i)
