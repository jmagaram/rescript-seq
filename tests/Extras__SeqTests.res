module T = Extras__Test
module S = Extras__Seq
module R = Extras__Result
module Ex = Extras
module Option = Belt.Option
module Result = Belt.Result

let concatInts = xs =>
  xs->Js.Array2.length == 0 ? "_" : xs->Js.Array2.map(Belt.Int.toString)->Js.Array2.joinWith("")

let areEqual = (~title, ~expectation, ~a, ~b) =>
  T.make(~category="Seq", ~title, ~expectation, ~predicate=() => {
    let a = a()->S.toArray
    if a != b {
      Js.Console.log(`===== NOT EQUAL : ${title} : ${expectation} =====`)
      Js.Console.log(`A: ${a->Js.Array2.toString}`)
      Js.Console.log(`B: ${b->Js.Array2.toString}`)
    }
    a == b
  })

let willThrow = (~title, ~expectation, ~f) => {
  T.make(~category="Seq", ~title, ~expectation, ~predicate=() => {
    Ex.Result.fromTryCatch(f)->Result.isError
  })
}

let consumeEqual = (~title, ~expectation, ~a, ~b) =>
  T.make(~category="Seq", ~title, ~expectation, ~predicate=() => {
    let aValue = a()
    aValue == b
  })

let oneTwoThree = S.init(~count=3, (~index) => index + 1)
let fourFiveSix = S.init(~count=3, (~index) => index + 4)
let oneToFive = S.init(~count=5, (~index) => index + 1)

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
  areEqual(~title="cons", ~expectation="when a + empty => a", ~a=() => S.cons(1, S.empty), ~b=[1]),
  areEqual(
    ~title="cons",
    ~expectation="when a + bcd => abcd",
    ~a=() => S.cons(1, S.range(~start=2, ~end=4)),
    ~b=[1, 2, 3, 4],
  ),
  areEqual(
    ~title="init",
    ~expectation="when count is < 0 => empty",
    ~a=() => S.init(~count=-1, (~index) => index->Belt.Int.toString),
    ~b=[],
  ),
  areEqual(
    ~title="init",
    ~expectation="when count is 0 => empty",
    ~a=() => S.init(~count=0, (~index) => index->Belt.Int.toString),
    ~b=[],
  ),
  areEqual(
    ~title="init",
    ~expectation="when count is 1 => singleton",
    ~a=() => S.init(~count=1, (~index) => index->Belt.Int.toString),
    ~b=["0"],
  ),
  areEqual(
    ~title="init",
    ~expectation="when count > 2 => map each index",
    ~a=() => S.init(~count=3, (~index) => index->Belt.Int.toString),
    ~b=["0", "1", "2"],
  ),
  areEqual(
    ~title="repeat",
    ~expectation="when count = 0 => empty",
    ~a=() => S.repeat(~count=0, ~value="x"),
    ~b=[],
  ),
  areEqual(
    ~title="repeat",
    ~expectation="when count = 1 => the item as singleton",
    ~a=() => S.repeat(~count=1, ~value="x"),
    ~b=["x"],
  ),
  areEqual(
    ~title="repeat",
    ~expectation="when count > 2 => the item repeated",
    ~a=() => S.repeat(~count=3, ~value="x"),
    ~b=["x", "x", "x"],
  ),
  T.make(~category="Seq", ~title="infinite", ~expectation="calls fn every time", ~predicate=() => {
    let count = 99999
    let minUnique = (0.95 *. count->Belt.Int.toFloat)->Belt.Int.fromFloat
    let unique =
      S.infinite(_ => Js.Math.random_int(0, 999999))
      ->S.takeAtMost(count)
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
      ->S.takeAtMost(count)
      ->S.toArray
      ->Belt.Set.Int.fromArray
      ->Belt.Set.Int.size
    unique > minUnique
  }),
  areEqual(
    ~title="fromArray",
    ~expectation="when not empty",
    ~a=() => [1, 2, 3]->S.fromArray,
    ~b=[1, 2, 3],
  ),
  areEqual(~title="fromArray", ~expectation="when one item", ~a=() => [1]->S.fromArray, ~b=[1]),
  areEqual(~title="fromArray", ~expectation="when empty", ~a=() => []->S.fromArray, ~b=[]),
  areEqual(
    ~title="fromArray",
    ~expectation="when end but no start",
    ~a=() => [0, 1, 2, 3]->S.fromArray(~start=0, ~end=2),
    ~b=[0, 1, 2],
  ),
  areEqual(
    ~title="fromArray",
    ~expectation="when end but no start",
    ~a=() => [0, 1, 2, 3]->S.fromArray(~end=2),
    ~b=[0, 1, 2],
  ),
  areEqual(
    ~title="fromArray",
    ~expectation="when neither start or end",
    ~a=() => [0, 1, 2, 3]->S.fromArray,
    ~b=[0, 1, 2, 3],
  ),
  areEqual(
    ~title="fromArray",
    ~expectation="when start => return start to end",
    ~a=() => [0, 1, 2, 3]->S.fromArray(~start=2),
    ~b=[2, 3],
  ),
  areEqual(
    ~title="fromList",
    ~expectation="",
    ~a=() => list{1, 2, 3, 4, 5}->S.fromList,
    ~b=[1, 2, 3, 4, 5],
  ),
  areEqual(
    ~title="fromString",
    ~expectation="get letters",
    ~a=() => "abc"->S.fromString,
    ~b=["a", "b", "c"],
  ),
  areEqual(
    ~title="iterate",
    ~expectation="",
    ~a=() => S.iterate(2, i => i * 2)->S.takeAtMost(3),
    ~b=[2, 4, 8],
  ),
  areEqual(~title="cycle", ~expectation="when empty => empty", ~a=() => S.empty, ~b=[]),
  areEqual(
    ~title="cycle",
    ~expectation="when singleton => repeat endlessly",
    ~a=() => S.singleton(1)->S.cycle->S.takeAtMost(5),
    ~b=[1, 1, 1, 1, 1],
  ),
  T.make(
    ~title="Seq",
    ~category="cycle",
    ~expectation="use but do not cache first value",
    ~predicate=() => {
      let generated = []
      let items =
        S.infinite(() => {
          let r = Js.Math.random()
          generated->Js.Array2.push(r)->ignore
          r
        })
        ->S.cycle
        ->S.takeAtMost(3)
        ->S.toArray
      items == generated
    },
  ),
  areEqual(
    ~title="cycle",
    ~expectation="when not empty => repeat endlessly",
    ~a=() => oneTwoThree->S.cycle->S.takeAtMost(16),
    ~b=[1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1],
  ),
  areEqual(
    ~title="allPairs",
    ~expectation="",
    ~a=() => S.allPairs(oneTwoThree, fourFiveSix),
    ~b=[(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)],
  ),
  areEqual(
    ~title="range",
    ~expectation="can count up",
    ~a=() => S.range(~start=3, ~end=7),
    ~b=[3, 4, 5, 6, 7],
  ),
  areEqual(
    ~title="range",
    ~expectation="can count down",
    ~a=() => S.range(~start=7, ~end=3),
    ~b=[7, 6, 5, 4, 3],
  ),
  areEqual(
    ~title="range",
    ~expectation="can have single value",
    ~a=() => S.range(~start=3, ~end=3),
    ~b=[3],
  ),
  areEqual(
    ~title="fromOption",
    ~expectation="when None => empty",
    ~a=() => None->S.fromOption,
    ~b=[],
  ),
  areEqual(
    ~title="fromOption",
    ~expectation="when Some => singleton",
    ~a=() => Some(1)->S.fromOption,
    ~b=[1],
  ),
]

let transforming = [
  areEqual(
    ~title="concat",
    ~expectation="when both not empty",
    ~a=() => S.concat(oneTwoThree, oneTwoThree),
    ~b=[1, 2, 3, 1, 2, 3],
  ),
  areEqual(
    ~title="concat",
    ~expectation="when both empty",
    ~a=() => S.concat(S.empty, S.empty),
    ~b=[],
  ),
  areEqual(
    ~title="concat",
    ~expectation="when first empty",
    ~a=() => S.concat(S.empty, oneTwoThree),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="concat",
    ~expectation="when second empty",
    ~a=() => S.concat(oneTwoThree, S.empty),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="flatMap",
    ~expectation="when map to several items => flatten",
    ~a=() => oneTwoThree->S.flatMap(i => S.repeat(~count=3, ~value=i)),
    ~b=[1, 1, 1, 2, 2, 2, 3, 3, 3],
  ),
  areEqual(
    ~title="flatMap",
    ~expectation="when original is empty => empty",
    ~a=() => S.empty->S.flatMap(_ => S.repeat(~count=5, ~value="x")),
    ~b=[],
  ),
  areEqual(
    ~title="flatMap",
    ~expectation="when original is one item",
    ~a=() => S.singleton(1)->S.flatMap(i => S.repeat(~count=5, ~value=i)),
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
    ~title="mapi",
    ~expectation="",
    ~a=() => oneTwoThree->S.mapi((~value, ~index) => (value, index)),
    ~b=[(1, 0), (2, 1), (3, 2)],
  ),
  areEqual(
    ~title="indexed",
    ~expectation="when items, index starts at 0",
    ~a=() => oneTwoThree->S.indexed,
    ~b=[(1, 0), (2, 1), (3, 2)],
  ),
  areEqual(
    ~title="takeAtMost",
    ~expectation="when 0 => empty",
    ~a=() => oneTwoThree->S.takeAtMost(0),
    ~b=[],
  ),
  areEqual(
    ~title="takeAtMost",
    ~expectation="when empty => empty",
    ~a=() => S.empty->S.takeAtMost(99),
    ~b=[],
  ),
  areEqual(
    ~title="takeAtMost",
    ~expectation="when n = all => all",
    ~a=() => oneTwoThree->S.takeAtMost(3),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="takeAtMost",
    ~expectation="when n > all => all",
    ~a=() => oneTwoThree->S.takeAtMost(9),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="takeAtMost",
    ~expectation="when a subset",
    ~a=() => oneTwoThree->S.takeAtMost(2),
    ~b=[1, 2],
  ),
  areEqual(
    ~title="takeWhile",
    ~expectation="when some match, return them",
    ~a=() => oneToFive->S.takeWhile(i => i <= 3),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="takeWhile",
    ~expectation="when none match, return empty",
    ~a=() => oneToFive->S.takeWhile(_ => false),
    ~b=[],
  ),
  areEqual(
    ~title="takeWhile",
    ~expectation="when empty, return empty",
    ~a=() => S.empty->S.takeWhile(_ => true),
    ~b=[],
  ),
  areEqual(
    ~title="takeWhile",
    ~expectation="when only first matches, return it",
    ~a=() => oneToFive->S.takeWhile(i => i == 1),
    ~b=[1],
  ),
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
  areEqual(
    ~title="filteri",
    ~expectation="",
    ~a=() => oneToFive->S.filteri((~value, ~index) => value == 3 && index == 2),
    ~b=[3],
  ),
  areEqual(
    ~title="zipLongest",
    ~expectation="when same length",
    ~a=() => S.zipLongest(oneTwoThree, oneTwoThree),
    ~b=[(Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3))],
  ),
  areEqual(
    ~title="zipLongest",
    ~expectation="when second longer => fill with None",
    ~a=() => S.zipLongest(oneTwoThree, oneToFive),
    ~b=[
      (Some(1), Some(1)),
      (Some(2), Some(2)),
      (Some(3), Some(3)),
      (None, Some(4)),
      (None, Some(5)),
    ],
  ),
  areEqual(
    ~title="zipLongest",
    ~expectation="when first longer => fill with None",
    ~a=() => S.zipLongest(oneToFive, oneTwoThree),
    ~b=[
      (Some(1), Some(1)),
      (Some(2), Some(2)),
      (Some(3), Some(3)),
      (Some(4), None),
      (Some(5), None),
    ],
  ),
  areEqual(
    ~title="zipLongest",
    ~expectation="when both empty => empty",
    ~a=() => S.zipLongest(S.empty, S.empty),
    ~b=[],
  ),
  areEqual(
    ~title="zipLongest",
    ~expectation="when None of different length",
    ~a=() => S.zipLongest(S.repeat(~count=3, ~value=None), S.repeat(~count=1, ~value=None)),
    ~b=[(Some(None), Some(None)), (Some(None), None), (Some(None), None)],
  ),
  areEqual(
    ~title="zip",
    ~expectation="when first longer, ignore excess",
    ~a=() => S.zip(oneToFive, oneTwoThree),
    ~b=[(1, 1), (2, 2), (3, 3)],
  ),
  areEqual(
    ~title="zip",
    ~expectation="when second longer, ignore excess",
    ~a=() => S.zip(oneTwoThree, oneToFive),
    ~b=[(1, 1), (2, 2), (3, 3)],
  ),
  areEqual(
    ~title="zip",
    ~expectation="when same length, combine",
    ~a=() => S.zip(oneTwoThree, fourFiveSix),
    ~b=[(1, 4), (2, 5), (3, 6)],
  ),
  areEqual(
    ~title="filterMap",
    ~expectation="keep the Some",
    ~a=() => oneToFive->S.filterMap(i => i == 2 || i == 3 ? Some(i * 2) : None),
    ~b=[4, 6],
  ),
  areEqual(
    ~title="filterMap",
    ~expectation="when no matches",
    ~a=() => oneToFive->S.filterMap(_ => None),
    ~b=[],
  ),
  areEqual(
    ~title="filterMap",
    ~expectation="when empty => empty",
    ~a=() => S.empty->S.filterMap(_ => Some(1)),
    ~b=[],
  ),
  areEqual(
    ~title="scani",
    ~expectation="when empty => zero",
    ~a=() => S.empty->S.scani(~zero=10, (~sum, ~value, ~index) => sum + value + index + 1),
    ~b=[10],
  ),
  areEqual(
    ~title="scani",
    ~expectation="when not empty => sequence of sums including zero",
    ~a=() => oneTwoThree->S.scani(~zero=10, (~sum, ~value, ~index) => sum + value + index),
    ~b=[10, 10 + 1 + 0, 10 + 1 + 0 + 2 + 1, 10 + 1 + 0 + 2 + 1 + 3 + 2],
  ),
  areEqual(
    ~title="dropWhile",
    ~expectation="start when predicate is true",
    ~a=() => oneTwoThree->S.dropWhile(i => i <= 2),
    ~b=[3],
  ),
  areEqual(
    ~title="dropWhile",
    ~expectation="start when predicate is true",
    ~a=() => oneTwoThree->S.dropWhile(_ => false),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="flatten",
    ~expectation="concatenate each sub-sequence",
    ~a=() => S.init(~count=3, (~index) => S.repeat(~count=2, ~value=index))->S.flatten,
    ~b=[0, 0, 1, 1, 2, 2],
  ),
  areEqual(
    ~title="map2",
    ~expectation="when same length, map all",
    ~a=() => S.map2(oneTwoThree, fourFiveSix, (a, b) => a * b),
    ~b=[4, 10, 18],
  ),
  areEqual(
    ~title="map2",
    ~expectation="when first is shorter, ignore excess",
    ~a=() => S.map2(oneTwoThree, oneToFive, (a, b) => a * b),
    ~b=[1, 4, 9],
  ),
  areEqual(
    ~title="map2",
    ~expectation="when second is shorter, ignore excess",
    ~a=() => S.map2(oneToFive, oneTwoThree, (a, b) => a * b),
    ~b=[1, 4, 9],
  ),
  areEqual(
    ~title="sortedMerge",
    ~expectation="",
    ~a=() =>
      S.sortedMerge(
        [1, 4, 4, 6, 7, 9, 11]->S.fromArray,
        [2, 3, 3, 5, 7, 10, 12]->S.fromArray,
        Ex.Cmp.int,
      ),
    ~b=[1, 2, 3, 3, 4, 4, 5, 6, 7, 7, 9, 10, 11, 12],
  ),
  areEqual(
    ~title="sortedMerge",
    ~expectation="first empty",
    ~a=() => S.sortedMerge([]->S.fromArray, [2, 3, 3, 5, 7, 10, 12]->S.fromArray, Ex.Cmp.int),
    ~b=[2, 3, 3, 5, 7, 10, 12],
  ),
  areEqual(
    ~title="sortedMerge",
    ~expectation="second empty",
    ~a=() => S.sortedMerge([1, 2, 2, 4]->S.fromArray, S.empty, Ex.Cmp.int),
    ~b=[1, 2, 2, 4],
  ),
  areEqual(~title="startWith", ~expectation="when empty", ~a=() => S.empty->S.startWith(1), ~b=[1]),
  areEqual(
    ~title="startWith",
    ~expectation="item not empty",
    ~a=() => [2, 3, 4]->S.fromArray->S.startWith(1),
    ~b=[1, 2, 3, 4],
  ),
  areEqual(
    ~title="prepend",
    ~expectation="",
    ~a=() => oneToFive->S.prepend(fourFiveSix),
    ~b=[4, 5, 6, 1, 2, 3, 4, 5],
  ),
  areEqual(
    ~title="concatMany",
    ~expectation="",
    ~a=() => oneToFive->S.concatMany(S.repeat(~count=3, ~value=oneTwoThree)),
    ~b=[1, 2, 3, 4, 5, 1, 2, 3, 1, 2, 3, 1, 2, 3],
  ),
  T.make(~category="Seq", ~title="tap", ~expectation="can inspect each value", ~predicate=() => {
    let seen = []
    let items = oneToFive->S.tap(i => seen->Js.Array2.push(i)->ignore)->S.toArray
    seen == [1, 2, 3, 4, 5] && items == [1, 2, 3, 4, 5]
  }),
  areEqual(
    ~title="filterSome",
    ~expectation="",
    ~a=() => [None, None, None]->S.fromArray->S.filterSome,
    ~b=[],
  ),
  areEqual(
    ~title="filterSome",
    ~expectation="",
    ~a=() => [Some(3), None, Some(5)]->S.fromArray->S.filterSome,
    ~b=[3, 5],
  ),
  areEqual(
    ~title="filterSome",
    ~expectation="when all None",
    ~a=() => [None, None, None]->S.fromArray->S.filterSome,
    ~b=[],
  ),
  areEqual(
    ~title="filterOk",
    ~expectation="",
    ~a=() => [Ok(3), Error("oops"), Ok(5)]->S.fromArray->S.filterOk,
    ~b=[3, 5],
  ),
  areEqual(
    ~title="chunkBySize",
    ~expectation="when not empty and longer than chunk size",
    ~a=() => [1, 2, 3, 4, 5, 6, 7]->S.fromArray->S.chunkBySize(3),
    ~b=[[1, 2, 3], [4, 5, 6], [7]],
  ),
  areEqual(
    ~title="chunkBySize",
    ~expectation="when not empty and shorter than chunk size",
    ~a=() => [1, 2, 3]->S.fromArray->S.chunkBySize(6),
    ~b=[[1, 2, 3]],
  ),
  areEqual(
    ~title="chunkBySize",
    ~expectation="when not empty and equal to chunk size",
    ~a=() => [1, 2, 3]->S.fromArray->S.chunkBySize(3),
    ~b=[[1, 2, 3]],
  ),
  T.make(
    ~category="Seq",
    ~title="chunkBySize",
    ~expectation="when size = 0 => throw",
    ~predicate=() => R.fromTryCatch(() => [1, 2, 3]->S.fromArray->S.chunkBySize(0))->Result.isError,
  ),
  T.make(
    ~category="Seq",
    ~title="chunkBySize",
    ~expectation="when size < 0 => throw",
    ~predicate=() =>
      R.fromTryCatch(() => [1, 2, 3]->S.fromArray->S.chunkBySize(-1))->Result.isError,
  ),
  areEqual(
    ~title="chunkBySize",
    ~expectation="when empty => empty",
    ~a=() => []->S.fromArray->S.chunkBySize(3),
    ~b=[],
  ),
  areEqual(
    ~title="window",
    ~expectation="when empty => empty",
    ~a=() => S.empty->S.window(5),
    ~b=[],
  ),
  T.make(~category="Seq", ~title="window", ~expectation="when size = 0 => throw", ~predicate=() =>
    R.fromTryCatch(() => [1, 2, 3]->S.fromArray->S.window(0))->Result.isError
  ),
  T.make(~category="Seq", ~title="window", ~expectation="when size < 0 => throw", ~predicate=() =>
    R.fromTryCatch(() => [1, 2, 3]->S.fromArray->S.window(-1))->Result.isError
  ),
  areEqual(
    ~title="window",
    ~expectation="when size < length",
    ~a=() => oneToFive->S.window(3)->S.map(concatInts),
    ~b=["123", "234", "345"],
  ),
  areEqual(
    ~title="window",
    ~expectation="when size = length",
    ~a=() => oneToFive->S.window(5)->S.map(concatInts),
    ~b=["12345"],
  ),
  areEqual(
    ~title="window",
    ~expectation="when size > length => empty",
    ~a=() => oneToFive->S.window(6)->S.map(concatInts),
    ~b=[],
  ),
  areEqual(
    ~title="windowAhead",
    ~expectation="when empty and size > 1",
    ~a=() => S.empty->S.windowAhead(3)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=[],
  ),
  areEqual(
    ~title="windowAhead",
    ~expectation="when empty and size = 1",
    ~a=() => S.empty->S.windowAhead(1)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=[],
  ),
  areEqual(
    ~title="windowAhead",
    ~expectation="when not empty and size > length",
    ~a=() => oneTwoThree->S.windowAhead(9)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["123", "23", "3"],
  ),
  areEqual(
    ~title="windowAhead",
    ~expectation="when not empty and size < length",
    ~a=() => oneTwoThree->S.windowAhead(2)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["12", "23", "3"],
  ),
  areEqual(
    ~title="windowAhead",
    ~expectation="when not empty and size = length",
    ~a=() => oneTwoThree->S.windowAhead(3)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["123", "23", "3"],
  ),
  areEqual(
    ~title="windowAhead",
    ~expectation="when singleton and size > length",
    ~a=() => S.singleton(1)->S.windowAhead(9)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1"],
  ),
  areEqual(
    ~title="windowAhead",
    ~expectation="when singleton and size = 1",
    ~a=() => S.singleton(1)->S.windowAhead(1)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1"],
  ),
  willThrow(~title="windowAhead", ~expectation="when size == 0 => throw", ~f=() =>
    oneToFive->S.windowAhead(0)
  ),
  willThrow(~title="windowAhead", ~expectation="when size < 0 => throw", ~f=() =>
    oneToFive->S.windowAhead(-1)
  ),
  areEqual(
    ~title="windowBehind",
    ~expectation="when empty and size > 1",
    ~a=() => S.empty->S.windowBehind(3)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=[],
  ),
  areEqual(
    ~title="windowBehind",
    ~expectation="when empty and size = 1",
    ~a=() => S.empty->S.windowBehind(1)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=[],
  ),
  areEqual(
    ~title="windowBehind",
    ~expectation="when not empty and size > length",
    ~a=() => oneTwoThree->S.windowBehind(9)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1", "12", "123"],
  ),
  areEqual(
    ~title="windowBehind",
    ~expectation="when not empty and size < length",
    ~a=() => oneTwoThree->S.windowBehind(2)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1", "12", "23"],
  ),
  areEqual(
    ~title="windowBehind",
    ~expectation="when not empty and size = length",
    ~a=() => oneTwoThree->S.windowBehind(3)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1", "12", "123"],
  ),
  areEqual(
    ~title="windowBehind",
    ~expectation="when singleton and size > length",
    ~a=() => S.singleton(1)->S.windowBehind(9)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1"],
  ),
  areEqual(
    ~title="windowBehind",
    ~expectation="when singleton and size = 1",
    ~a=() => S.singleton(1)->S.windowBehind(1)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1"],
  ),
  willThrow(~title="windowBehind", ~expectation="when size == 0 => throw", ~f=() =>
    oneToFive->S.windowBehind(0)
  ),
  willThrow(~title="windowBehind", ~expectation="when size < 0 => throw", ~f=() =>
    oneToFive->S.windowBehind(-1)
  ),
  areEqual(
    ~title="pairwise",
    ~expectation="when empty => empty",
    ~a=() => S.empty->S.pairwise,
    ~b=[],
  ),
  areEqual(
    ~title="pairwise",
    ~expectation="when singleton => None",
    ~a=() => S.singleton(5)->S.pairwise,
    ~b=[],
  ),
  areEqual(
    ~title="pairwise",
    ~expectation="when multiple",
    ~a=() => oneTwoThree->S.pairwise,
    ~b=[(1, 2), (2, 3)],
  ),
  areEqual(
    ~title="intersperse",
    ~expectation="when empty => empty",
    ~a=() => S.empty->S.intersperse(-1),
    ~b=[],
  ),
  areEqual(
    ~title="intersperse",
    ~expectation="when singleton => singleton",
    ~a=() => S.singleton(5)->S.intersperse(-1),
    ~b=[5],
  ),
  areEqual(
    ~title="intersperse",
    ~expectation="when two",
    ~a=() => [1, 2]->S.fromArray->S.intersperse(-1),
    ~b=[1, -1, 2],
  ),
  areEqual(
    ~title="intersperse",
    ~expectation="when many",
    ~a=() => [1, 2, 3, 4]->S.fromArray->S.intersperse(-1),
    ~b=[1, -1, 2, -1, 3, -1, 4],
  ),
  areEqual(
    ~title="interleave",
    ~expectation="when first shorter",
    ~a=() => S.interleave(fourFiveSix, oneToFive),
    ~b=[4, 1, 5, 2, 6, 3, 4, 5],
  ),
  areEqual(
    ~title="interleave",
    ~expectation="when second shorter",
    ~a=() => S.interleave(oneToFive, fourFiveSix),
    ~b=[1, 4, 2, 5, 3, 6, 4, 5],
  ),
  areEqual(
    ~title="interleave",
    ~expectation="when second empty",
    ~a=() => S.interleave(oneToFive, S.empty),
    ~b=[1, 2, 3, 4, 5],
  ),
  areEqual(
    ~title="interleave",
    ~expectation="when first empty",
    ~a=() => S.interleave(S.empty, oneToFive),
    ~b=[1, 2, 3, 4, 5],
  ),
  areEqual(
    ~title="interleave",
    ~expectation="when both empty",
    ~a=() => S.interleave(S.empty, S.empty),
    ~b=[],
  ),
  areEqual(
    ~title="interleaveMany",
    ~expectation="when one singleton",
    ~a=() => S.interleaveMany([S.singleton(1)]),
    ~b=[1],
  ),
  areEqual(
    ~title="interleaveMany",
    ~expectation="when one",
    ~a=() => S.interleaveMany([oneTwoThree]),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="interleaveMany",
    ~expectation="when two",
    ~a=() => S.interleaveMany([oneTwoThree, fourFiveSix]),
    ~b=[1, 4, 2, 5, 3, 6],
  ),
  areEqual(
    ~title="interleaveMany",
    ~expectation="when two and first longer",
    ~a=() => S.interleaveMany([oneToFive, fourFiveSix]),
    ~b=[1, 4, 2, 5, 3, 6, 4, 5],
  ),
  areEqual(
    ~title="interleaveMany",
    ~expectation="when two and second longer",
    ~a=() => S.interleaveMany([fourFiveSix, oneToFive]),
    ~b=[4, 1, 5, 2, 6, 3, 4, 5],
  ),
  areEqual(
    ~title="interleaveMany",
    ~expectation="when many",
    ~a=() => S.interleaveMany([oneToFive, oneTwoThree, fourFiveSix]),
    ~b=[1, 1, 4, 2, 2, 5, 3, 3, 6, 4, 5],
  ),
  areEqual(
    ~title="interleaveMany",
    ~expectation="when many",
    ~a=() => S.interleaveMany([S.empty, oneToFive, S.empty, oneTwoThree, S.empty, fourFiveSix]),
    ~b=[1, 1, 4, 2, 2, 5, 3, 3, 6, 4, 5],
  ),
  areEqual(
    ~title="takeUntil",
    ~expectation="when more than one item and condition met, that is last item",
    ~a=() => oneToFive->S.takeUntil(i => i == 3),
    ~b=[1, 2, 3],
  ),
  areEqual(
    ~title="takeUntil",
    ~expectation="when more than one item and condition not met, return all items",
    ~a=() => oneToFive->S.takeUntil(_ => false),
    ~b=[1, 2, 3, 4, 5],
  ),
  areEqual(
    ~title="takeUntil",
    ~expectation="when one item and condition met, that is last item",
    ~a=() => S.singleton(1)->S.takeUntil(i => i == 1),
    ~b=[1],
  ),
  areEqual(
    ~title="takeUntil",
    ~expectation="when one item and condition not met, return all items",
    ~a=() => S.singleton(1)->S.takeUntil(i => i == 99),
    ~b=[1],
  ),
  areEqual(
    ~title="takeUntil",
    ~expectation="when empty => empty",
    ~a=() => S.empty->S.takeUntil(_ => true),
    ~b=[],
  ),
  areEqual(
    ~title="dropUntil",
    ~expectation="when more than one item and condition met, that is first item",
    ~a=() => oneToFive->S.dropUntil(i => i == 1),
    ~b=[1, 2, 3, 4, 5],
  ),
  areEqual(
    ~title="dropUntil",
    ~expectation="when more than one item and condition met, that is first item",
    ~a=() => oneToFive->S.dropUntil(i => i == 4),
    ~b=[4, 5],
  ),
  areEqual(
    ~title="dropUntil",
    ~expectation="when more than one item and condition not met, return no items",
    ~a=() => oneToFive->S.dropUntil(i => i == 99),
    ~b=[],
  ),
  areEqual(
    ~title="dropUntil",
    ~expectation="when empty => empty",
    ~a=() => S.empty->S.dropUntil(_ => true),
    ~b=[],
  ),
  areEqual(
    ~title="dropUntil",
    ~expectation="when singleton and condition not met => empty",
    ~a=() => S.singleton(1)->S.dropUntil(i => i == 99),
    ~b=[],
  ),
  areEqual(
    ~title="dropUntil",
    ~expectation="when singleton and condition met => singleton",
    ~a=() => S.singleton(1)->S.dropUntil(i => i == 1),
    ~b=[1],
  ),
]

let consuming = [
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
      S.repeat(~count=1000, ~value=0)
      ->S.flatMap(_ => S.repeat(~count=1000, ~value=0))
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
    ~title="findMapi",
    ~expectation="",
    ~a=() => oneToFive->S.findMapi((~value, ~index) => value == 3 && index == 2 ? Some("x") : None),
    ~b=Some("x"),
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
  consumeEqual(
    ~title="equals",
    ~expectation="if shorter first => false",
    ~a=() => S.equals(oneTwoThree, oneToFive, (x: int, y: int) => x == y),
    ~b=false,
  ),
  consumeEqual(
    ~title="equals",
    ~expectation="if longer first => false",
    ~a=() => S.equals(oneToFive, oneTwoThree, (x: int, y: int) => x == y),
    ~b=false,
  ),
  consumeEqual(
    ~title="equals",
    ~expectation="if same values => true",
    ~a=() => S.equals(oneTwoThree, oneTwoThree, (x: int, y: int) => x == y),
    ~b=true,
  ),
  consumeEqual(
    ~title="equals",
    ~expectation="if different values => false",
    ~a=() => S.equals(S.singleton(1), S.singleton(2), (x: int, y: int) => x == y),
    ~b=false,
  ),
  consumeEqual(
    ~title="equals",
    ~expectation="can compare different types",
    ~a=() =>
      S.equals(S.singleton(1), S.singleton("1"), (x: int, y: string) => x->Belt.Int.toString == y),
    ~b=true,
  ),
  consumeEqual(
    ~title="equals",
    ~expectation="can compare different types",
    ~a=() =>
      S.equals(S.singleton(1), S.singleton("1"), (x: int, y: string) => x->Belt.Int.toString == y),
    ~b=true,
  ),
  consumeEqual(
    ~title="compare",
    ~expectation="",
    ~a=() => S.compare([1, 2, 3]->S.fromArray, [1, 2, 3]->S.fromArray, Ex.Cmp.int),
    ~b=0,
  ),
  consumeEqual(
    ~title="compare",
    ~expectation="",
    ~a=() => S.compare([1, 2, 3]->S.fromArray, [1, 2, 3, 4]->S.fromArray, Ex.Cmp.int),
    ~b=-1,
  ),
  consumeEqual(
    ~title="compare",
    ~expectation="",
    ~a=() => S.compare([1, 2, 3, 4]->S.fromArray, [1, 2, 3]->S.fromArray, Ex.Cmp.int),
    ~b=1,
  ),
  consumeEqual(
    ~title="compare",
    ~expectation="",
    ~a=() => S.compare([]->S.fromArray, []->S.fromArray, Ex.Cmp.int),
    ~b=0,
  ),
  consumeEqual(
    ~title="compare",
    ~expectation="",
    ~a=() => S.compare([1, 2, 3, 4]->S.fromArray, [1, 3, 3, 4]->S.fromArray, Ex.Cmp.int),
    ~b=-1,
  ),
  consumeEqual(
    ~title="headTail",
    ~expectation="when empty => None",
    ~a=() => S.empty->S.headTail,
    ~b=None,
  ),
  consumeEqual(
    ~title="headTail",
    ~expectation="when singleton => Some(head,empty)",
    ~a=() => S.singleton(4)->S.headTail->Option.map(((h, t)) => (h, t->S.toArray)),
    ~b=Some(4, []),
  ),
  consumeEqual(
    ~title="headTail",
    ~expectation="when many items => Some(head,tail)",
    ~a=() => [1, 2, 3]->S.fromArray->S.headTail->Option.map(((h, t)) => (h, t->S.toArray)),
    ~b=Some(1, [2, 3]),
  ),
  consumeEqual(~title="head", ~expectation="when empty", ~a=() => S.empty->S.head, ~b=None),
  consumeEqual(
    ~title="head",
    ~expectation="when not empty",
    ~a=() => oneToFive->S.head,
    ~b=Some(1),
  ),
  consumeEqual(
    ~title="minBy",
    ~expectation="when no items => None",
    ~a=() => S.empty->S.minBy(Ex.Cmp.int),
    ~b=None,
  ),
  consumeEqual(
    ~title="minBy",
    ~expectation="when items => Some",
    ~a=() => [6, 7, 8, 3, 1, 3, 5, 8]->S.fromArray->S.minBy(Ex.Cmp.int),
    ~b=Some(1),
  ),
  consumeEqual(
    ~title="maxBy",
    ~expectation="when no items => None",
    ~a=() => S.empty->S.maxBy(Ex.Cmp.int),
    ~b=None,
  ),
  consumeEqual(
    ~title="maxBy",
    ~expectation="when items => Some",
    ~a=() => [6, 7, 8, 3, 1, 3, 5, 7]->S.fromArray->S.maxBy(Ex.Cmp.int),
    ~b=Some(8),
  ),
  consumeEqual(
    ~title="isEmpty",
    ~expectation="when empty => true",
    ~a=() => S.empty->S.isEmpty,
    ~b=true,
  ),
  consumeEqual(
    ~title="isEmpty",
    ~expectation="when not empty => false",
    ~a=() => S.singleton(2)->S.isEmpty,
    ~b=false,
  ),
  consumeEqual(~title="last", ~expectation="when empty => None", ~a=() => S.empty->S.last, ~b=None),
  consumeEqual(
    ~title="last",
    ~expectation="when singleton",
    ~a=() => S.singleton(1)->S.last,
    ~b=Some(1),
  ),
  consumeEqual(~title="last", ~expectation="when many", ~a=() => oneToFive->S.last, ~b=Some(5)),
  consumeEqual(
    ~title="toString",
    ~expectation="",
    ~a=() => ["a", "b", "c"]->S.fromArray->S.toString,
    ~b="abc",
  ),
  consumeEqual(
    ~title="toString",
    ~expectation="when empty",
    ~a=() => []->S.fromArray->S.toString,
    ~b="",
  ),
  consumeEqual(
    ~title="toExactlyOne",
    ~expectation="when empty => None",
    ~a=() => S.empty->S.toExactlyOne,
    ~b=None,
  ),
  consumeEqual(
    ~title="toExactlyOne",
    ~expectation="when singleton => Some",
    ~a=() => S.singleton(1)->S.toExactlyOne,
    ~b=Some(1),
  ),
  consumeEqual(
    ~title="toExactlyOne",
    ~expectation="when many => None",
    ~a=() => oneTwoThree->S.toExactlyOne,
    ~b=None,
  ),
  consumeEqual(
    ~title="isSortedBy",
    ~expectation="when empty => true",
    ~a=() => S.empty->S.isSortedBy(Ex.Cmp.int),
    ~b=true,
  ),
  consumeEqual(
    ~title="isSortedBy",
    ~expectation="when singleton => true",
    ~a=() => S.singleton(4)->S.isSortedBy(Ex.Cmp.int),
    ~b=true,
  ),
  consumeEqual(
    ~title="isSortedBy",
    ~expectation="when sorted => true",
    ~a=() => [1, 2, 2, 3, 4, 5]->S.fromArray->S.isSortedBy(Ex.Cmp.int),
    ~b=true,
  ),
  consumeEqual(
    ~title="isSortedBy",
    ~expectation="when not sorted => false",
    ~a=() => [1, 2, 2, 3, 4, 2, 5]->S.fromArray->S.isSortedBy(Ex.Cmp.int),
    ~b=false,
  ),
  consumeEqual(
    ~title="toOption",
    ~expectation="when empty => None",
    ~a=() => S.empty->S.toOption,
    ~b=None,
  ),
  consumeEqual(
    ~title="toOption",
    ~expectation="when singleton => Some",
    ~a=() => S.singleton(1)->S.toOption->Option.getWithDefault(S.empty)->S.toArray,
    ~b=[1],
  ),
  consumeEqual(
    ~title="toOption",
    ~expectation="when many items => Some",
    ~a=() => oneTwoThree->S.toOption->Option.getWithDefault(S.empty)->S.toArray,
    ~b=[1, 2, 3],
  ),
]

let validationTests = [
  ("when empty, return empty", [], Ok([])),
  ("when one error, return it", [30], Error("30")),
  ("when one ok, return it", [2], Ok([4])),
  ("when all ok, return all", [1, 2, 3], Ok([2, 4, 6])),
  ("when all error, return first", [20, 30, 40], Error("20")),
  ("when mix, return first error", [1, 2, 14, 3, 4], Error("14")),
]

let validate = n => n < 10 ? Ok(n * 2) : Error(n->Belt.Int.toString)

let allOkTests =
  validationTests->Belt.Array.map(((expectation, input, expected)) =>
    consumeEqual(
      ~title="allOk",
      ~expectation,
      ~a=() => input->S.fromArray->S.allOk(validate)->Result.map(i => i->S.toArray),
      ~b=expected,
    )
  )

let allSomeTests = {
  validationTests->Belt.Array.map(((expectation, input, expected)) =>
    consumeEqual(
      ~title="allSome",
      ~expectation,
      ~a=() =>
        input
        ->S.fromArray
        ->S.allSome(i => i->validate->Ex.Result.toOption)
        ->Option.map(i => i->S.toArray),
      ~b=expected->Ex.Result.toOption,
    )
  )
}

let memoizeTests = [
  T.make(
    ~category="Seq",
    ~title="cache",
    ~expectation="calculations only done once",
    ~predicate=() => {
      let randoms = S.infinite(() => Js.Math.random())->S.takeAtMost(4)->S.cache
      let nums1 = randoms->S.toArray
      let nums2 = randoms->S.toArray
      let nums3 = randoms->S.toArray
      nums1 == nums2 && nums2 == nums3
    },
  ),
  T.make(
    ~category="Seq",
    ~title="cache",
    ~expectation="all lazy; can cache infinite",
    ~predicate=() => {
      let randoms = S.infinite(() => Js.Math.random())->S.cache->S.takeAtMost(4)
      let nums1 = randoms->S.toArray
      let nums2 = randoms->S.toArray
      let nums3 = randoms->S.toArray
      nums1 == nums2 && nums2 == nums3
    },
  ),
]

let tests =
  [
    allOkTests,
    allSomeTests,
    constructors,
    transforming,
    consuming,
    memoizeTests,
  ]->Belt.Array.flatMap(i => i)
