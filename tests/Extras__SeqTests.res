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

let consumeEqual = (~title, ~expectation, ~a, ~b) =>
  T.make(~category="Seq", ~title, ~expectation, ~predicate=() => {
    let aValue = a()
    aValue == b
  })

let oneTwoThree = S.init(~count=3, ~initializer=(~index) => index + 1)
let oneToFive = S.init(~count=5, ~initializer=(~index) => index + 1)

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
  T.make(~category="Seq", ~title="infinite", ~expectation="calls fn every time", ~predicate=() => {
    let count = 99999
    let minUnique = (0.95 *. count->Belt.Int.toFloat)->Belt.Int.fromFloat
    let unique =
      S.infinite(_ => Js.Math.random_int(0, 999999))
      ->S.take(count)
      ->S.toArray
      ->Belt.Set.Int.fromArray
      ->Belt.Set.Int.size
    unique > minUnique
  }),
  T.make(~category="Seq", ~title="infinite", ~expectation="stack won't overflow", ~predicate=() => {
    let count = 99999
    let minUnique = (0.95 *. count->Belt.Int.toFloat)->Belt.Int.fromFloat
    let unique =
      S.infinite(_ => Js.Math.random_int(0, 999999))
      ->S.take(count)
      ->S.toArray
      ->Belt.Set.Int.fromArray
      ->Belt.Set.Int.size
    unique > minUnique
  }),
]

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
  areEqual(
    ~title="map",
    ~expectation="",
    ~a=() => oneTwoThree->S.map(Belt.Int.toString),
    ~b=["1", "2", "3"],
  ),
  areEqual(
    ~title="indexed",
    ~expectation="when items, index starts at 0",
    ~a=() => oneTwoThree->S.indexed,
    ~b=[(1, 0), (2, 1), (3, 2)],
  ),
  areEqual(~title="take", ~expectation="when 0 => empty", ~a=() => oneTwoThree->S.take(0), ~b=[]),
  areEqual(~title="take", ~expectation="when empty => empty", ~a=() => S.empty->S.take(99), ~b=[]),
  areEqual(
    ~title="take",
    ~expectation="when n = all => all",
    ~a=() => oneTwoThree->S.take(3),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="take",
    ~expectation="when n > all => all",
    ~a=() => oneTwoThree->S.take(9),
    ~b=[1, 2, 3],
  ),
  areEqual(~title="take", ~expectation="when a subset", ~a=() => oneTwoThree->S.take(2), ~b=[1, 2]),
  areEqual(
    ~title="drop",
    ~expectation="when zero => original seq",
    ~a=() => oneToFive->S.drop(0),
    ~b=[1, 2, 3, 4, 5],
  ),
  areEqual(~title="drop", ~expectation="when all => empty", ~a=() => oneToFive->S.drop(5), ~b=[]),
  areEqual(~title="drop", ~expectation="when subset", ~a=() => oneToFive->S.drop(2), ~b=[3, 4, 5]),
  areEqual(
    ~title="filter",
    ~expectation="",
    ~a=() => oneToFive->S.filter(i => i == 2 || i == 5),
    ~b=[2, 5],
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
  T.make(~category="Seq", ~title="forEach", ~expectation="", ~predicate=() => {
    let result = []
    oneToFive->S.forEach(i => result->Js.Array2.push(i)->ignore)
    result == [1, 2, 3, 4, 5]
  }),
  T.make(~category="Seq", ~title="forEachi", ~expectation="", ~predicate=() => {
    let result = []
    oneToFive->S.forEachi((~value, ~index) => result->Js.Array2.push((value, index))->ignore)
    result == [(1, 0), (2, 1), (3, 2), (4, 3), (5, 4)]
  }),
  T.make(
    ~category="Seq",
    ~title="unfold",
    ~expectation="a million stack won't overflow",
    ~predicate=() => {
      S.unfold(0, i => i < 999999 ? Some(i, i + 1) : None)->S.forEach(_ => ())
      true
    },
  ),
  T.make(
    ~category="Seq",
    ~title="flatMap",
    ~expectation="a million stack won't overflow",
    ~predicate=() => {
      S.replicate(~count=1000, ~value=0)
      ->S.flatMap(_ => S.replicate(~count=1000, ~value=0))
      ->S.forEach(_ => ())
      true
    },
  ),
  consumeEqual(
    ~title="some",
    ~expectation="if empty => false",
    ~a=() => S.empty->S.some(_ => true),
    ~b=false,
  ),
  consumeEqual(
    ~title="some",
    ~expectation="if some predicate true => true",
    ~a=() => oneToFive->S.some(i => i == 2),
    ~b=true,
  ),
  consumeEqual(
    ~title="some",
    ~expectation="if no predicate true => false",
    ~a=() => oneToFive->S.some(i => i == 99),
    ~b=false,
  ),
  consumeEqual(
    ~title="every",
    ~expectation="if empty => true",
    ~a=() => S.empty->S.everyOrEmpty(_ => false),
    ~b=true,
  ),
  consumeEqual(
    ~title="every",
    ~expectation="if all true => true",
    ~a=() => oneToFive->S.everyOrEmpty(i => i >= 1 && i <= 5),
    ~b=true,
  ),
  consumeEqual(
    ~title="every",
    ~expectation="if any false => false",
    ~a=() => oneToFive->S.everyOrEmpty(i => i != 3),
    ~b=false,
  ),
  consumeEqual(
    ~title="find",
    ~expectation="if not found => None",
    ~a=() => oneToFive->S.find(i => i == 99),
    ~b=None,
  ),
  consumeEqual(
    ~title="find",
    ~expectation="if found => Some",
    ~a=() => oneToFive->S.find(i => i == 2),
    ~b=Some(2),
  ),
  consumeEqual(
    ~title="findMap",
    ~expectation="if found => Some",
    ~a=() => oneToFive->S.findMap(i => i == 2 ? Some("x") : None),
    ~b=Some("x"),
  ),
  consumeEqual(
    ~title="findMap",
    ~expectation="if not found => None",
    ~a=() => oneToFive->S.findMap(i => i == 99 ? Some("x") : None),
    ~b=None,
  ),
  consumeEqual(
    ~title="reduce",
    ~expectation="if empty => initial value",
    ~a=() => S.empty->S.reduce(99, (sum, i) => sum * i),
    ~b=99,
  ),
  consumeEqual(
    ~title="reduce",
    ~expectation="if 1 item => f of the item and initial value",
    ~a=() => S.singleton(4)->S.reduce(5, (sum, i) => sum * i),
    ~b=20,
  ),
  consumeEqual(
    ~title="reduce",
    ~expectation="if many items => f of all the items and initial value",
    ~a=() => oneToFive->S.reduce(-1, (sum, i) => sum * i),
    ~b=-1 * 2 * 3 * 4 * 5,
  ),
  consumeEqual(
    ~title="reduce",
    ~expectation="can transform",
    ~a=() => oneToFive->S.reduce("", (sum, i) => `${i->Belt.Int.toString}${sum}`),
    ~b="54321",
  ),
  consumeEqual(
    ~title="reducei",
    ~expectation="if many items => f of all the items and initial value",
    ~a=() => oneTwoThree->S.reducei(0, (~sum, ~value, ~index) => sum + value * (index + 1)),
    ~b=1 * 1 + 2 * 2 + 3 * 3,
  ),
  consumeEqual(~title="length", ~expectation="if empty => 0", ~a=() => S.empty->S.length, ~b=0),
  consumeEqual(~title="length", ~expectation="if not empty", ~a=() => oneToFive->S.length, ~b=5),
]

let tests = [constructors, transforming, consuming]->Belt.Array.flatMap(i => i)
