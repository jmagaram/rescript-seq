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

let tests = [constructors, consuming]->Belt.Array.flatMap(i => i)
