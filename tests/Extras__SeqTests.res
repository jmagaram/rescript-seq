module T = Extras__Test
module S = Extras__Seq
module R = Extras__Result
module Ex = Extras
module Option = Belt.Option
module Result = Belt.Result

let intToString = Belt.Int.toString

let concatInts = xs =>
  xs->Js.Array2.length == 0 ? "_" : xs->Js.Array2.map(intToString)->Js.Array2.joinWith("")

let shorten = s => Js.String2.slice(s, ~from=0, ~to_=1000)

let trueAlways = _ => true

let falseAlways = _ => false

let oneTwoThree = S.init(~count=3, (~index) => index + 1)
let fourFiveSix = S.init(~count=3, (~index) => index + 4)
let oneToFive = S.init(~count=5, (~index) => index + 1)

let seqEqual = (~title, ~expectation, ~a, ~b) =>
  T.make(~category="Seq", ~title, ~expectation, ~predicate=() => {
    let a = a()->S.toArray
    if a != b {
      Js.Console.log(`===== NOT EQUAL : ${title} : ${expectation} =====`)
      Js.Console.log(`A: ${a->Js.Array2.toString->shorten}`)
      Js.Console.log(`B: ${b->Js.Array2.toString->shorten}`)
    }
    a == b
  })

let willThrow = (~title, ~expectation, ~f) =>
  T.make(~category="Seq", ~title, ~expectation, ~predicate=() => {
    Ex.Result.fromTryCatch(f)->Result.isError
  })

let foldEqual = (~title, ~expectation, ~a, ~b) =>
  T.make(~category="Seq", ~title, ~expectation, ~predicate=() => {
    let aValue = a()
    let pass = aValue == b
    if !pass {
      Js.Console.log(`===== NOT EQUAL : ${title} : ${expectation} =====`)
      Js.Console.log(`A: ${a->Obj.magic}`)
      Js.Console.log(`B: ${b->Obj.magic}`)
    }
    pass
  })

let makeSeqEqualsTests = (~title, xs) =>
  xs->Js.Array2.mapi(((source, result, note), inx) =>
    seqEqual(~title, ~expectation=`index ${inx->intToString} ${note}`, ~a=() => source, ~b=result)
  )

let basicConstructorTests = [
  seqEqual(~title="singleton", ~expectation="has one item in it", ~a=() => S.singleton(3), ~b=[3]),
  seqEqual(~title="empty", ~expectation="has no items", ~a=() => S.empty, ~b=[]),
  seqEqual(~title="cons", ~expectation="when a + empty => a", ~a=() => S.cons(1, S.empty), ~b=[1]),
  seqEqual(
    ~title="cons",
    ~expectation="when a + bcd => abcd",
    ~a=() => S.cons(1, S.range(~start=2, ~end=4)),
    ~b=[1, 2, 3, 4],
  ),
  seqEqual(
    ~title="fromOption",
    ~expectation="when None => empty",
    ~a=() => None->S.fromOption,
    ~b=[],
  ),
  seqEqual(
    ~title="fromOption",
    ~expectation="when Some => singleton",
    ~a=() => Some(1)->S.fromOption,
    ~b=[1],
  ),
  seqEqual(
    ~title="fromString",
    ~expectation="get letters",
    ~a=() => "abc"->S.fromString,
    ~b=["a", "b", "c"],
  ),
  seqEqual(~title="startWith", ~expectation="when empty", ~a=() => S.empty->S.startWith(1), ~b=[1]),
  seqEqual(
    ~title="startWith",
    ~expectation="item not empty",
    ~a=() => [2, 3, 4]->S.fromArray->S.startWith(1),
    ~b=[1, 2, 3, 4],
  ),
]

let fromListTests = [
  seqEqual(
    ~title="fromList",
    ~expectation="",
    ~a=() => list{1, 2, 3, 4, 5}->S.fromList,
    ~b=[1, 2, 3, 4, 5],
  ),
]

let cycleTests = [
  seqEqual(~title="cycle", ~expectation="when empty => empty", ~a=() => S.empty, ~b=[]),
  seqEqual(
    ~title="cycle",
    ~expectation="when singleton => repeat endlessly",
    ~a=() => S.singleton(1)->S.cycle->S.takeAtMost(5),
    ~b=[1, 1, 1, 1, 1],
  ),
  seqEqual(
    ~title="cycle",
    ~expectation="when infinite => can still cycle",
    ~a=() => S.infinite(() => 1)->S.cycle->S.takeAtMost(5),
    ~b=[1, 1, 1, 1, 1],
  ),
  seqEqual(
    ~title="cycle",
    ~expectation="first item is cached and used",
    ~a=() => {
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
      S.singleton(items[0] == generated[0])
    },
    ~b=[true],
  ),
  seqEqual(
    ~title="cycle",
    ~expectation="when not empty => repeat endlessly",
    ~a=() => oneTwoThree->S.cycle->S.takeAtMost(16),
    ~b=[1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1],
  ),
]

let allPairsTests = makeSeqEqualsTests(
  ~title="allPairs",
  [
    (S.allPairs(S.empty, S.empty), [], ""),
    (S.allPairs(S.empty, fourFiveSix), [], ""),
    (S.allPairs(S.singleton(1), S.singleton(2)), [(1, 2)], ""),
    (S.allPairs(S.singleton(1), fourFiveSix), [(1, 4), (1, 5), (1, 6)], ""),
    (S.allPairs(fourFiveSix, S.singleton(1)), [(4, 1), (5, 1), (6, 1)], ""),
    (
      S.allPairs(oneTwoThree, fourFiveSix),
      [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)],
      "",
    ),
  ],
)

let rangeTests = [
  seqEqual(
    ~title="range",
    ~expectation="can count up",
    ~a=() => S.range(~start=3, ~end=7),
    ~b=[3, 4, 5, 6, 7],
  ),
  seqEqual(
    ~title="range",
    ~expectation="can count down",
    ~a=() => S.range(~start=7, ~end=3),
    ~b=[7, 6, 5, 4, 3],
  ),
  seqEqual(
    ~title="range",
    ~expectation="can have single value",
    ~a=() => S.range(~start=3, ~end=3),
    ~b=[3],
  ),
]

let concatTests = [
  seqEqual(
    ~title="concat",
    ~expectation="when both not empty",
    ~a=() => S.concat(oneTwoThree, oneTwoThree),
    ~b=[1, 2, 3, 1, 2, 3],
  ),
  seqEqual(
    ~title="concat",
    ~expectation="when both empty",
    ~a=() => S.concat(S.empty, S.empty),
    ~b=[],
  ),
  seqEqual(
    ~title="concat",
    ~expectation="when first empty",
    ~a=() => S.concat(S.empty, oneTwoThree),
    ~b=[1, 2, 3],
  ),
  seqEqual(
    ~title="concat",
    ~expectation="when second empty",
    ~a=() => S.concat(oneTwoThree, S.empty),
    ~b=[1, 2, 3],
  ),
]

let flatMapTests = [
  seqEqual(
    ~title="flatMap",
    ~expectation="when map to several items => flatten",
    ~a=() => oneTwoThree->S.flatMap(i => S.replicate(~count=3, ~value=i)),
    ~b=[1, 1, 1, 2, 2, 2, 3, 3, 3],
  ),
  seqEqual(
    ~title="flatMap",
    ~expectation="when original is empty => empty",
    ~a=() => S.empty->S.flatMap(_ => S.replicate(~count=5, ~value="x")),
    ~b=[],
  ),
  seqEqual(
    ~title="flatMap",
    ~expectation="when original is one item",
    ~a=() => S.singleton(1)->S.flatMap(i => S.replicate(~count=5, ~value=i)),
    ~b=[1, 1, 1, 1, 1],
  ),
  seqEqual(
    ~title="flatMap",
    ~expectation="when mapped result is empty => empty",
    ~a=() => oneTwoThree->S.flatMap(_ => S.empty),
    ~b=[],
  ),
  seqEqual(
    ~title="flatMap",
    ~expectation="when mapped result is one item",
    ~a=() => oneTwoThree->S.flatMap(i => S.singleton(i->intToString)),
    ~b=["1", "2", "3"],
  ),
]->Js.Array2.concat([
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
])

let mapTests = makeSeqEqualsTests(
  ~title="map",
  [
    (oneToFive->S.map(i => i + 1), [2, 3, 4, 5, 6], ""),
    (S.singleton(1)->S.map(i => i + 1), [2], ""),
    (S.empty->S.map(i => i + 1), [], ""),
    (oneTwoThree->S.mapi((~value, ~index) => value * index), [0, 2, 6], ""),
  ],
)

let indexedTests = makeSeqEqualsTests(
  ~title="indexed",
  [
    (oneTwoThree->S.indexed, [(1, 0), (2, 1), (3, 2)], ""),
    (S.singleton(9)->S.indexed, [(9, 0)], ""),
    (S.empty->S.indexed, [], ""),
  ],
)

let takeWhileTests = [
  seqEqual(
    ~title="takeWhile",
    ~expectation="when some match, return them",
    ~a=() => oneToFive->S.takeWhile(i => i <= 3),
    ~b=[1, 2, 3],
  ),
  seqEqual(
    ~title="takeWhile",
    ~expectation="when none match, return empty",
    ~a=() => oneToFive->S.takeWhile(_ => false),
    ~b=[],
  ),
  seqEqual(
    ~title="takeWhile",
    ~expectation="when empty, return empty",
    ~a=() => S.empty->S.takeWhile(_ => true),
    ~b=[],
  ),
  seqEqual(
    ~title="takeWhile",
    ~expectation="when only first matches, return it",
    ~a=() => oneToFive->S.takeWhile(i => i == 1),
    ~b=[1],
  ),
]

let dropTests = [
  seqEqual(
    ~title="drop",
    ~expectation="when count = 0 => original seq by value",
    ~a=() => oneToFive->S.drop(0),
    ~b=[1, 2, 3, 4, 5],
  ),
  seqEqual(
    ~title="drop",
    ~expectation="when count = 0 => original seq by ===",
    ~a=() => S.singleton(oneToFive->S.drop(0) === oneToFive),
    ~b=[true],
  ),
  seqEqual(
    ~title="drop",
    ~expectation="when count == length => empty",
    ~a=() => oneToFive->S.drop(5),
    ~b=[],
  ),
  seqEqual(
    ~title="drop",
    ~expectation="when count > length => empty",
    ~a=() => oneToFive->S.drop(Int32.max_int),
    ~b=[],
  ),
  seqEqual(
    ~title="drop",
    ~expectation="when count < length => subset",
    ~a=() => oneToFive->S.drop(2),
    ~b=[3, 4, 5],
  ),
  seqEqual(
    ~title="drop",
    ~expectation="when count == 1 => just skip first",
    ~a=() => oneToFive->S.drop(1),
    ~b=[2, 3, 4, 5],
  ),
  seqEqual(
    ~title="drop",
    ~expectation="when drop a million items => no stack overflow",
    ~a=() => S.concat(S.replicate(~count=999_999, ~value="x"), S.singleton("y"))->S.drop(999_999),
    ~b=["y"],
  ),
]

let flattenTests = [
  seqEqual(
    ~title="flatten",
    ~expectation="concatenate each sub-sequence",
    ~a=() => S.init(~count=3, (~index) => S.replicate(~count=2, ~value=index))->S.flatten,
    ~b=[0, 0, 1, 1, 2, 2],
  ),
  seqEqual(
    ~title="flatten",
    ~expectation="concatenate each sub-sequence",
    ~a=() => S.infinite(() => S.empty)->S.takeAtMost(5)->S.flatten,
    ~b=[],
  ),
]

let sortedMergeTests = [
  seqEqual(
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
  seqEqual(
    ~title="sortedMerge",
    ~expectation="first empty",
    ~a=() => S.sortedMerge([]->S.fromArray, [2, 3, 3, 5, 7, 10, 12]->S.fromArray, Ex.Cmp.int),
    ~b=[2, 3, 3, 5, 7, 10, 12],
  ),
  seqEqual(
    ~title="sortedMerge",
    ~expectation="second empty",
    ~a=() => S.sortedMerge([1, 2, 2, 4]->S.fromArray, S.empty, Ex.Cmp.int),
    ~b=[1, 2, 2, 4],
  ),
]

let prependTests = [
  seqEqual(
    ~title="prepend",
    ~expectation="",
    ~a=() => oneToFive->S.prepend(fourFiveSix),
    ~b=[4, 5, 6, 1, 2, 3, 4, 5],
  ),
]

let tapTests = [
  T.make(~category="Seq", ~title="tap", ~expectation="can inspect each value", ~predicate=() => {
    let seen = []
    let items = oneToFive->S.tap(i => seen->Js.Array2.push(i)->ignore)->S.toArray
    seen == [1, 2, 3, 4, 5] && items == [1, 2, 3, 4, 5]
  }),
]

let windowTests = [
  seqEqual(
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
  seqEqual(
    ~title="window",
    ~expectation="when size < length",
    ~a=() => oneToFive->S.window(3)->S.map(concatInts),
    ~b=["123", "234", "345"],
  ),
  seqEqual(
    ~title="window",
    ~expectation="when size = length",
    ~a=() => oneToFive->S.window(5)->S.map(concatInts),
    ~b=["12345"],
  ),
  seqEqual(
    ~title="window",
    ~expectation="when size > length => empty",
    ~a=() => oneToFive->S.window(6)->S.map(concatInts),
    ~b=[],
  ),
]

let windowAheadTests = [
  seqEqual(
    ~title="windowAhead",
    ~expectation="when empty and size > 1",
    ~a=() => S.empty->S.windowAhead(3)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=[],
  ),
  seqEqual(
    ~title="windowAhead",
    ~expectation="when empty and size = 1",
    ~a=() => S.empty->S.windowAhead(1)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=[],
  ),
  seqEqual(
    ~title="windowAhead",
    ~expectation="when not empty and size > length",
    ~a=() => oneTwoThree->S.windowAhead(9)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["123", "23", "3"],
  ),
  seqEqual(
    ~title="windowAhead",
    ~expectation="when not empty and size < length",
    ~a=() => oneTwoThree->S.windowAhead(2)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["12", "23", "3"],
  ),
  seqEqual(
    ~title="windowAhead",
    ~expectation="when not empty and size = length",
    ~a=() => oneTwoThree->S.windowAhead(3)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["123", "23", "3"],
  ),
  seqEqual(
    ~title="windowAhead",
    ~expectation="when singleton and size > length",
    ~a=() => S.singleton(1)->S.windowAhead(9)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1"],
  ),
  seqEqual(
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
]

let windowBehindTests = [
  seqEqual(
    ~title="windowBehind",
    ~expectation="when empty and size > 1",
    ~a=() => S.empty->S.windowBehind(3)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=[],
  ),
  seqEqual(
    ~title="windowBehind",
    ~expectation="when empty and size = 1",
    ~a=() => S.empty->S.windowBehind(1)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=[],
  ),
  seqEqual(
    ~title="windowBehind",
    ~expectation="when not empty and size > length",
    ~a=() => oneTwoThree->S.windowBehind(9)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1", "12", "123"],
  ),
  seqEqual(
    ~title="windowBehind",
    ~expectation="when not empty and size < length",
    ~a=() => oneTwoThree->S.windowBehind(2)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1", "12", "23"],
  ),
  seqEqual(
    ~title="windowBehind",
    ~expectation="when not empty and size = length",
    ~a=() => oneTwoThree->S.windowBehind(3)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1", "12", "123"],
  ),
  seqEqual(
    ~title="windowBehind",
    ~expectation="when singleton and size > length",
    ~a=() => S.singleton(1)->S.windowBehind(9)->S.map(i => i->Js.Array2.copy->concatInts),
    ~b=["1"],
  ),
  seqEqual(
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
]

let pairwiseTests = [
  seqEqual(
    ~title="pairwise",
    ~expectation="when empty => empty",
    ~a=() => S.empty->S.pairwise,
    ~b=[],
  ),
  seqEqual(
    ~title="pairwise",
    ~expectation="when singleton => None",
    ~a=() => S.singleton(5)->S.pairwise,
    ~b=[],
  ),
  seqEqual(
    ~title="pairwise",
    ~expectation="when multiple",
    ~a=() => oneTwoThree->S.pairwise,
    ~b=[(1, 2), (2, 3)],
  ),
]

let interleaveTests = [
  seqEqual(
    ~title="interleave",
    ~expectation="when first shorter",
    ~a=() => S.interleave(fourFiveSix, oneToFive),
    ~b=[4, 1, 5, 2, 6, 3, 4, 5],
  ),
  seqEqual(
    ~title="interleave",
    ~expectation="when second shorter",
    ~a=() => S.interleave(oneToFive, fourFiveSix),
    ~b=[1, 4, 2, 5, 3, 6, 4, 5],
  ),
  seqEqual(
    ~title="interleave",
    ~expectation="when second empty",
    ~a=() => S.interleave(oneToFive, S.empty),
    ~b=[1, 2, 3, 4, 5],
  ),
  seqEqual(
    ~title="interleave",
    ~expectation="when first empty",
    ~a=() => S.interleave(S.empty, oneToFive),
    ~b=[1, 2, 3, 4, 5],
  ),
  seqEqual(
    ~title="interleave",
    ~expectation="when both empty",
    ~a=() => S.interleave(S.empty, S.empty),
    ~b=[],
  ),
]

// ISSUE - Can't write my tests to validate length is expected because toArray
// might call an extra unfold method. Maybe this is a real bug.
let interleaveManyTests = {
  let manualTests = [
    seqEqual(
      ~title="interleaveMany",
      ~expectation="when one singleton",
      ~a=() => S.interleaveMany([S.singleton(1)]),
      ~b=[1],
    ),
    seqEqual(
      ~title="interleaveMany",
      ~expectation="when one",
      ~a=() => S.interleaveMany([oneTwoThree]),
      ~b=[1, 2, 3],
    ),
    seqEqual(
      ~title="interleaveMany",
      ~expectation="when two",
      ~a=() => S.interleaveMany([oneTwoThree, fourFiveSix]),
      ~b=[1, 4, 2, 5, 3, 6],
    ),
    seqEqual(
      ~title="interleaveMany",
      ~expectation="when two and first longer",
      ~a=() => S.interleaveMany([oneToFive, fourFiveSix]),
      ~b=[1, 4, 2, 5, 3, 6, 4, 5],
    ),
    seqEqual(
      ~title="interleaveMany",
      ~expectation="when two and second longer",
      ~a=() => S.interleaveMany([fourFiveSix, oneToFive]),
      ~b=[4, 1, 5, 2, 6, 3, 4, 5],
    ),
    seqEqual(
      ~title="interleaveMany",
      ~expectation="when many",
      ~a=() => S.interleaveMany([oneToFive, oneTwoThree, fourFiveSix]),
      ~b=[1, 1, 4, 2, 2, 5, 3, 3, 6, 4, 5],
    ),
    seqEqual(
      ~title="interleaveMany",
      ~expectation="when many",
      ~a=() => S.interleaveMany([S.empty, oneToFive, S.empty, oneTwoThree, S.empty, fourFiveSix]),
      ~b=[1, 1, 4, 2, 2, 5, 3, 3, 6, 4, 5],
    ),
  ]
  manualTests
}

let iterateTests = makeSeqEqualsTests(
  ~title="iterate",
  [
    (S.iterate(2, i => i * 2)->S.takeAtMost(3), [2, 4, 8], ""),
    (S.iterate(2, i => i * 2)->S.takeAtMost(1), [2], ""),
    (S.iterate(2, i => i * 2)->S.takeAtMost(0), [], ""),
    (
      S.iterate(1, i => i + 1)->S.takeAtMost(999_999)->S.filter(i => i === 999_999),
      [999_999],
      "millions",
    ),
  ],
)

let fromArrayTests = {
  let basicTests = makeSeqEqualsTests(
    ~title="fromArray",
    [
      ([1, 2, 3]->S.fromArray, [1, 2, 3], ""),
      ([1]->S.fromArray, [1], ""),
      ([]->S.fromArray, [], ""),
      ([0, 1, 2, 3]->S.fromArray(~start=0, ~end=2), [0, 1, 2], ""),
      ([0, 1, 2, 3]->S.fromArray(~end=2), [0, 1, 2], ""),
      ([0, 1, 2, 3]->S.fromArray, [0, 1, 2, 3], ""),
      ([0, 1, 2, 3]->S.fromArray(~start=2), [2, 3], ""),
      ([0, 1, 2, 3]->S.fromArray(~start=2, ~end=2), [2], ""),
      ([0, 1, 2, 3]->S.fromArray(~start=2, ~end=1), [2, 1], ""),
    ],
  )
  let throws = f => willThrow(~title="fromArray", ~expectation="throws", ~f)
  let throwsTests = [
    throws(() => [1, 2, 3]->S.fromArray(~start=-1)),
    throws(() => [1, 2, 3]->S.fromArray(~start=3)),
    throws(() => []->S.fromArray(~start=0)),
    throws(() => []->S.fromArray(~end=0)),
    throws(() => [1, 2, 3]->S.fromArray(~end=-1)),
    throws(() => [1, 2, 3]->S.fromArray(~end=3)),
    throws(() => []->S.fromArray(~end=0)),
  ]
  Js.Array2.concat(basicTests, throwsTests)
}

let replicateTests = makeSeqEqualsTests(
  ~title="replicate",
  [
    (S.replicate(~count=0, ~value=1), [], ""),
    (S.replicate(~count=1, ~value=1), [1], ""),
    (S.replicate(~count=2, ~value=1), [1, 1], ""),
  ],
)

let takeAtMostTests = makeSeqEqualsTests(
  ~title="takeAtMost",
  [
    (S.empty->S.takeAtMost(0), [], ""),
    (S.empty->S.takeAtMost(1), [], ""),
    (S.singleton(1)->S.takeAtMost(0), [], ""),
    (S.singleton(1)->S.takeAtMost(1), [1], ""),
    (S.singleton(1)->S.takeAtMost(2), [1], ""),
    (oneToFive->S.takeAtMost(0), [], ""),
    (oneToFive->S.takeAtMost(1), [1], ""),
    (oneToFive->S.takeAtMost(3), [1, 2, 3], ""),
    (oneToFive->S.takeAtMost(5), [1, 2, 3, 4, 5], ""),
    (oneToFive->S.takeAtMost(6), [1, 2, 3, 4, 5], ""),
  ],
)->Js.Array2.concat([
  foldEqual(
    ~title="takeAtMost",
    ~expectation="millions",
    ~a=() => S.range(~start=1, ~end=999_999)->S.last,
    ~b=Some(999_999),
  ),
  T.make(
    ~category="Seq",
    ~title="takeAtMost",
    ~expectation="if zero completely lazy",
    ~predicate=() => {
      S.unfold(0, _ => {
        Js.Exn.raiseError("oops!")
      })
      ->S.takeAtMost(0)
      ->S.toArray
      ->ignore
      true
    },
  ),
  T.make(
    ~category="Seq",
    ~title="takeAtMost",
    ~expectation="if 3, generator function called 3 times",
    ~predicate=() => {
      let callCount = ref(0)
      S.unfold(0, _ => {
        callCount := callCount.contents + 1
        Some(1, 1)
      })
      ->S.takeAtMost(3)
      ->S.toArray
      ->ignore
      callCount.contents == 3 + 1 // extra for the toArray to find the end?
    },
  ),
])

let infiniteTests = [
  seqEqual(
    ~title="infinite",
    ~expectation="values are generated",
    ~a=() => S.infinite(() => 1)->S.takeAtMost(5),
    ~b=[1, 1, 1, 1, 1],
  ),
  T.make(~category="Seq", ~title="infinite", ~expectation="millions", ~predicate=() => {
    let callCount = ref(0)
    S.infinite(() => {
      callCount := callCount.contents + 1
      callCount.contents
    })
    ->S.takeAtMost(999_999)
    ->S.forEach(_ => ())
    callCount.contents == 999_999 + 1
  }),
]

let unfoldTests =
  makeSeqEqualsTests(
    ~title="unfold",
    [
      (S.unfold(1, i => i <= 5 ? Some(i, i + 1) : None), [1, 2, 3, 4, 5], ""),
      (S.unfold(1, _ => None), [], "zero items"),
      (S.unfold(1, i => i < 100 ? Some(i, i * 2) : None), [1, 2, 4, 8, 16, 32, 64], ""),
    ],
  )->Js.Array2.concat([
    foldEqual(
      ~title="unfold",
      ~expectation="millions",
      ~a=() => S.unfold(1, i => i <= 999_999 ? Some(i, i + 1) : None)->S.last,
      ~b=Some(999_999),
    ),
  ])

let initTests =
  makeSeqEqualsTests(
    ~title="init",
    [
      (S.init(~count=1, (~index) => index * 2), [0], ""),
      (S.init(~count=2, (~index) => index * 2), [0, 2], ""),
      (S.init(~count=3, (~index) => index * 2), [0, 2, 4], ""),
    ],
  )->Js.Array2.concat([
    foldEqual(
      ~title="init",
      ~expectation="tens",
      ~a=() => S.init(~count=100, (~index) => index)->S.last,
      ~b=Some(99),
    ),
    foldEqual(
      ~title="init",
      ~expectation="millions",
      ~a=() => S.init(~count=1_000_000, (~index) => index)->S.last,
      ~b=Some(999_999),
    ),
  ])

let chunkBySizeTests = {
  let process = (xs, n) => xs->S.chunkBySize(n)->S.map(concatInts)
  makeSeqEqualsTests(
    ~title="chunkBySize",
    [
      (S.empty->process(1), [], ""),
      (S.singleton(1)->process(1), ["1"], ""),
      (S.singleton(1)->process(2), ["1"], ""),
      (oneTwoThree->process(1), ["1", "2", "3"], ""),
      (oneTwoThree->process(2), ["12", "3"], ""),
      (oneTwoThree->process(3), ["123"], ""),
      (oneTwoThree->process(4), ["123"], "millions"),
      (
        S.range(~start=0, ~end=9)
        ->S.cycle
        ->S.takeAtMost(1_000_000)
        ->S.chunkBySize(10)
        ->S.map(concatInts)
        ->S.last
        ->Option.map(S.singleton)
        ->Option.getWithDefault(""->S.singleton),
        ["0123456789"],
        "",
      ),
    ],
  )->Js.Array2.concat([
    willThrow(~title="chunkBySize", ~expectation="when size == 0", ~f=() =>
      oneToFive->S.chunkBySize(0)
    ),
    willThrow(~title="chunkBySize", ~expectation="when size == -1", ~f=() =>
      oneToFive->S.chunkBySize(0)
    ),
  ])
}

let scanTests = {
  let push = (sum, x) => {
    let copied = sum->Js.Array2.copy
    copied->Js.Array2.push(x)->ignore
    copied
  }
  let scanConcat = xs => xs->S.scan([0], push)->S.map(concatInts)
  makeSeqEqualsTests(
    ~title="scan",
    [
      (oneTwoThree->scanConcat, ["0", "01", "012", "0123"], ""),
      (S.singleton(1)->scanConcat, ["0", "01"], ""),
      (S.empty->scanConcat, ["0"], "always includes the zero"),
      (
        S.range(~start=1, ~end=999_999)
        ->S.scan(-1, (_, i) => i)
        ->S.map(intToString)
        ->S.last
        ->Option.map(s => S.singleton(s))
        ->Option.getWithDefault(S.singleton("")),
        ["999999"],
        "",
      ),
    ],
  )
}

let takeUntilTests = makeSeqEqualsTests(
  ~title="takeUntil",
  [
    (S.empty->S.takeUntil(trueAlways), [], ""),
    (S.empty->S.takeUntil(falseAlways), [], ""),
    (1->S.singleton->S.takeUntil(i => i == 1), [1], ""),
    (1->S.singleton->S.takeUntil(falseAlways), [1], ""),
    (1->S.singleton->S.takeUntil(trueAlways), [1], ""),
    (oneToFive->S.takeUntil(trueAlways), [1], ""),
    (oneToFive->S.takeUntil(falseAlways), [1, 2, 3, 4, 5], ""),
    (oneToFive->S.takeUntil(i => i == 3), [1, 2, 3], ""),
    (oneToFive->S.takeUntil(i => i == 1), [1], ""),
    (oneToFive->S.takeUntil(i => i == 5), [1, 2, 3, 4, 5], ""),
    ([1, 2, 2, 2, 3]->S.fromArray->S.takeUntil(i => i == 2), [1, 2], ""),
    (
      S.range(~start=1, ~end=99)
      ->S.takeUntil(i => i == 99)
      ->S.last
      ->Option.map(S.singleton)
      ->Option.getWithDefault(S.empty),
      [99],
      "tens",
    ),
    (
      S.range(~start=1, ~end=999_999)
      ->S.takeUntil(i => i == 999_999)
      ->S.last
      ->Option.map(S.singleton)
      ->Option.getWithDefault(S.empty),
      [999_999],
      "millions",
    ),
  ],
)

let intersperseTests = makeSeqEqualsTests(
  ~title="intersperse",
  [
    ([]->S.fromArray->S.intersperse(","), [], ""),
    (["a"]->S.fromArray->S.intersperse(","), ["a"], ""),
    (["a", "b"]->S.fromArray->S.intersperse(","), ["a", ",", "b"], ""),
    (["a", "b", "c"]->S.fromArray->S.intersperse(","), ["a", ",", "b", ",", "c"], ""),
  ],
)

let dropWhileTests =
  [
    (S.empty, falseAlways, [], ""),
    (S.empty, trueAlways, [], ""),
    (1->S.singleton, i => i == 1, [], ""),
    (1->S.singleton, falseAlways, [1], ""),
    (1->S.singleton, trueAlways, [], ""),
    (oneToFive, trueAlways, [], ""),
    (oneToFive, falseAlways, [1, 2, 3, 4, 5], ""),
    (oneToFive, i => i < 3, [3, 4, 5], ""),
    (oneToFive, i => i <= 5, [], ""),
    (oneToFive, i => i <= 1, [2, 3, 4, 5], ""),
    (S.range(~start=1, ~end=99), i => i != 99, [99], "tens"),
    (S.range(~start=1, ~end=9_999), i => i != 9_999, [9_999], "thousands"),
    (S.range(~start=1, ~end=999_999), i => i != 999_999, [999_999], "millions"),
  ]->Js.Array2.mapi(((source, predicate, result, note), inx) =>
    seqEqual(
      ~title="dropWhile",
      ~expectation=`index ${inx->intToString} ${note}`,
      ~a=() => source->S.dropWhile(predicate),
      ~b=result,
    )
  )

let filterSomeTests = makeSeqEqualsTests(
  ~title="filterSome",
  [
    (S.empty->S.filterSome, [], ""),
    (S.singleton(None)->S.filterSome, [], ""),
    (S.singleton(Some(1))->S.filterSome, [1], ""),
    ([Some(1), None, Some(3), None]->S.fromArray->S.filterSome, [1, 3], ""),
  ],
)

let filterOkTests = makeSeqEqualsTests(
  ~title="filterOk",
  [
    (S.empty->S.filterOk, [], ""),
    (S.singleton(Error("x"))->S.filterOk, [], ""),
    (S.singleton(Ok(1))->S.filterOk, [1], ""),
    ([Ok(1), Error("x"), Ok(3), Error("x")]->S.fromArray->S.filterOk, [1, 3], ""),
  ],
)

let filterMapTests =
  [
    (S.empty, _ => Some(1), [], ""),
    (S.empty, _ => None, [], ""),
    (1->S.singleton, i => Some(i * 2), [2], ""),
    (1->S.singleton, _ => None, [], ""),
    (oneToFive, i => Some(i), [1, 2, 3, 4, 5], ""),
    (oneToFive, _ => None, [], ""),
    (oneToFive, i => i == 3 || i == 5 ? Some(i) : None, [3, 5], ""),
    (oneToFive, i => i == 5 ? Some(99) : None, [99], ""),
    (oneToFive, i => i == 1 || i == 3 ? Some(i * 2) : None, [2, 6], ""),
    (S.range(~start=1, ~end=999_999), i => i == 999_999 ? Some(-i) : None, [-999_999], "millions"),
  ]->Js.Array2.mapi(((source, f, result, note), inx) =>
    seqEqual(
      ~title="filterMap",
      ~expectation=`index ${inx->intToString} ${note}`,
      ~a=() => source->S.filterMap(f),
      ~b=result,
    )
  )

let filterTests =
  [
    (S.empty, falseAlways, [], ""),
    (S.empty, trueAlways, [], ""),
    (1->S.singleton, i => i == 1, [1], ""),
    (1->S.singleton, falseAlways, [], ""),
    (1->S.singleton, trueAlways, [1], ""),
    (oneToFive, trueAlways, [1, 2, 3, 4, 5], ""),
    (oneToFive, falseAlways, [], ""),
    (oneToFive, i => i == 3 || i == 5, [3, 5], ""),
    (oneToFive, i => i == 5, [5], ""),
    (oneToFive, i => i == 1 || i == 3, [1, 3], ""),
    (S.range(~start=1, ~end=999_999), i => i == 999_999, [999_999], "millions"),
  ]
  ->Js.Array2.mapi(((source, predicate, result, note), inx) =>
    seqEqual(
      ~title="filter",
      ~expectation=`index ${inx->intToString} ${note}`,
      ~a=() => source->S.filter(predicate),
      ~b=result,
    )
  )
  ->Js.Array.concat([
    seqEqual(
      ~title="filteri",
      ~expectation="",
      ~a=() => oneToFive->S.filteri((~value, ~index) => value == 3 && index == 2),
      ~b=[3],
    ),
    seqEqual(
      ~title="filteri",
      ~expectation="when skipping millions => no stack problem",
      ~a=() =>
        S.replicate(~count=999_999, ~value=1)
        ->S.concat(2->S.singleton)
        ->S.filteri((~value, ~index as _) => value != 1),
      ~b=[2],
    ),
  ])

let dropUntilTests =
  [
    (S.empty, falseAlways, [], ""),
    (S.empty, trueAlways, [], ""),
    (1->S.singleton, i => i == 1, [1], ""),
    (1->S.singleton, falseAlways, [], ""),
    (1->S.singleton, trueAlways, [1], ""),
    (oneToFive, trueAlways, [1, 2, 3, 4, 5], ""),
    (oneToFive, falseAlways, [], ""),
    (oneToFive, i => i == 3, [3, 4, 5], ""),
    (oneToFive, i => i == 5, [5], ""),
    (oneToFive, i => i == 1, [1, 2, 3, 4, 5], ""),
    (S.range(~start=1, ~end=999_999), i => i == 999_999, [999_999], "millions"),
  ]->Js.Array2.mapi(((source, predicate, result, note), inx) =>
    seqEqual(
      ~title="dropUntil",
      ~expectation=`index ${inx->intToString} ${note}`,
      ~a=() => source->S.dropUntil(predicate),
      ~b=result,
    )
  )

let forEachTests = [
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
]

let someTests = [
  foldEqual(
    ~title="some",
    ~expectation="if empty => false",
    ~a=() => S.empty->S.some(_ => true),
    ~b=false,
  ),
  foldEqual(
    ~title="some",
    ~expectation="if some predicate true => true",
    ~a=() => oneToFive->S.some(i => i == 2),
    ~b=true,
  ),
  foldEqual(
    ~title="some",
    ~expectation="if no predicate true => false",
    ~a=() => oneToFive->S.some(i => i == 99),
    ~b=false,
  ),
  foldEqual(
    ~title="some",
    ~expectation="millions",
    ~a=() => S.range(~start=1, ~end=999_999)->S.some(i => i === 999_999),
    ~b=true,
  ),
]

let everyTests = [
  foldEqual(
    ~title="every",
    ~expectation="if empty => true",
    ~a=() => S.empty->S.everyOrEmpty(_ => false),
    ~b=true,
  ),
  foldEqual(
    ~title="every",
    ~expectation="if all true => true",
    ~a=() => oneToFive->S.everyOrEmpty(i => i >= 1 && i <= 5),
    ~b=true,
  ),
  foldEqual(
    ~title="every",
    ~expectation="if any false => false",
    ~a=() => oneToFive->S.everyOrEmpty(i => i != 3),
    ~b=false,
  ),
  foldEqual(
    ~title="every",
    ~expectation="millions",
    ~a=() => S.range(~start=1, ~end=999_999)->S.everyOrEmpty(i => i >= 1 && i <= 999_999),
    ~b=true,
  ),
]

let findMapTests = [
  foldEqual(
    ~title="findMap",
    ~expectation="if found => Some",
    ~a=() => oneToFive->S.findMap(i => i == 2 ? Some("x") : None),
    ~b=Some("x"),
  ),
  foldEqual(
    ~title="findMap",
    ~expectation="if not found => None",
    ~a=() => oneToFive->S.findMap(i => i == 99 ? Some("x") : None),
    ~b=None,
  ),
  foldEqual(
    ~title="findMapi",
    ~expectation="",
    ~a=() => oneToFive->S.findMapi((~value, ~index) => value == 3 && index == 2 ? Some("x") : None),
    ~b=Some("x"),
  ),
  foldEqual(
    ~title="findMap",
    ~expectation="millions",
    ~a=() => S.range(~start=1, ~end=999_999)->S.findMap(i => i == 999_999 ? Some("x") : None),
    ~b=Some("x"),
  ),
]

let toArrayTests = [
  foldEqual(
    ~title="toArray",
    ~expectation="when empty sequence, no generator functions are called",
    ~a=() => {
      let calls = ref(0)
      S.unfold(0, i => {
        calls := calls.contents + 1
        i < 100 ? Some(i, i + 1) : None
      })
      ->S.takeAtMost(0)
      ->S.toArray
      ->ignore
      calls.contents
    },
    ~b=0,
  ),
]

let lengthTests = [
  foldEqual(~title="length", ~expectation="if empty => 0", ~a=() => S.empty->S.length, ~b=0),
  foldEqual(~title="length", ~expectation="if not empty", ~a=() => oneToFive->S.length, ~b=5),
  foldEqual(
    ~title="length",
    ~expectation="millions",
    ~a=() => S.replicate(~count=999_999, ~value=1)->S.length,
    ~b=999_999,
  ),
]

let equalsTests = [
  ("", "", true),
  ("", "a", false),
  ("a", "", false),
  ("a", "a", true),
  ("a", "aa", false),
  ("aa", "a", false),
  ("aa", "aa", true),
  ("aa", "aaa", false),
]->Js.Array2.map(((xs, ys, expected)) =>
  foldEqual(
    ~title="equals",
    ~expectation=`${xs},${ys} => ${expected ? "true" : "false"}`,
    ~a=() => {
      let xs = xs->S.fromString
      let ys = ys->S.fromString
      S.equals(xs, ys, (i, j) => i == j)
    },
    ~b=expected,
  )
)

let compareTests = [
  ("", "", 0),
  ("", "a", -1),
  ("a", "", 1),
  ("a", "a", 0),
  ("a", "aa", -1),
  ("aa", "a", 1),
  ("aa", "aa", 0),
  ("aa", "aaa", -1),
]->Js.Array2.map(((xs, ys, expected)) =>
  foldEqual(
    ~title="compare",
    ~expectation=`${xs},${ys} => ${expected->intToString}`,
    ~a=() => {
      let xs = xs->S.fromString
      let ys = ys->S.fromString
      S.compare(xs, ys, Ex.Cmp.string)
    },
    ~b=expected,
  )
)

let headTailTests = [
  foldEqual(
    ~title="headTail",
    ~expectation="when empty => None",
    ~a=() => S.empty->S.headTail,
    ~b=None,
  ),
  foldEqual(
    ~title="headTail",
    ~expectation="when singleton => Some(head,empty)",
    ~a=() => S.singleton(4)->S.headTail->Option.map(((h, t)) => (h, t->S.toArray)),
    ~b=Some(4, []),
  ),
  foldEqual(
    ~title="headTail",
    ~expectation="when many items => Some(head,tail)",
    ~a=() => [1, 2, 3]->S.fromArray->S.headTail->Option.map(((h, t)) => (h, t->S.toArray)),
    ~b=Some(1, [2, 3]),
  ),
]

let headTests = [
  foldEqual(~title="head", ~expectation="when empty", ~a=() => S.empty->S.head, ~b=None),
  foldEqual(~title="head", ~expectation="when not empty", ~a=() => oneToFive->S.head, ~b=Some(1)),
]

let minByMaxByTests = [
  foldEqual(
    ~title="minBy",
    ~expectation="when no items => None",
    ~a=() => S.empty->S.minBy(Ex.Cmp.int),
    ~b=None,
  ),
  foldEqual(
    ~title="minBy",
    ~expectation="when items => Some",
    ~a=() => [6, 7, 8, 3, 1, 3, 5, 8]->S.fromArray->S.minBy(Ex.Cmp.int),
    ~b=Some(1),
  ),
  foldEqual(
    ~title="maxBy",
    ~expectation="when no items => None",
    ~a=() => S.empty->S.maxBy(Ex.Cmp.int),
    ~b=None,
  ),
  foldEqual(
    ~title="maxBy",
    ~expectation="when items => Some",
    ~a=() => [6, 7, 8, 3, 1, 3, 5, 7]->S.fromArray->S.maxBy(Ex.Cmp.int),
    ~b=Some(8),
  ),
]

let isEqualTests = [
  foldEqual(
    ~title="isEmpty",
    ~expectation="when empty => true",
    ~a=() => S.empty->S.isEmpty,
    ~b=true,
  ),
  foldEqual(
    ~title="isEmpty",
    ~expectation="when not empty => false",
    ~a=() => S.singleton(2)->S.isEmpty,
    ~b=false,
  ),
]

let toStringTests = [
  foldEqual(
    ~title="toString",
    ~expectation="",
    ~a=() => ["a", "b", "c"]->S.fromArray->S.toString,
    ~b="abc",
  ),
  foldEqual(
    ~title="toString",
    ~expectation="when empty",
    ~a=() => []->S.fromArray->S.toString,
    ~b="",
  ),
]

let toExactlyOneTests = [
  foldEqual(
    ~title="toExactlyOne",
    ~expectation="when empty => None",
    ~a=() => S.empty->S.toExactlyOne,
    ~b=None,
  ),
  foldEqual(
    ~title="toExactlyOne",
    ~expectation="when singleton => Some",
    ~a=() => S.singleton(1)->S.toExactlyOne,
    ~b=Some(1),
  ),
  foldEqual(
    ~title="toExactlyOne",
    ~expectation="when many => None",
    ~a=() => oneTwoThree->S.toExactlyOne,
    ~b=None,
  ),
]

let isSortedByTests = [
  foldEqual(
    ~title="isSortedBy",
    ~expectation="when empty => true",
    ~a=() => S.empty->S.isSortedBy(Ex.Cmp.int),
    ~b=true,
  ),
  foldEqual(
    ~title="isSortedBy",
    ~expectation="when singleton => true",
    ~a=() => S.singleton(4)->S.isSortedBy(Ex.Cmp.int),
    ~b=true,
  ),
  foldEqual(
    ~title="isSortedBy",
    ~expectation="when sorted => true",
    ~a=() => [1, 2, 2, 3, 4, 5]->S.fromArray->S.isSortedBy(Ex.Cmp.int),
    ~b=true,
  ),
  foldEqual(
    ~title="isSortedBy",
    ~expectation="when not sorted => false",
    ~a=() => [1, 2, 2, 3, 4, 2, 5]->S.fromArray->S.isSortedBy(Ex.Cmp.int),
    ~b=false,
  ),
]

let toOptionTests = [
  foldEqual(
    ~title="toOption",
    ~expectation="when empty => None",
    ~a=() => S.empty->S.toOption,
    ~b=None,
  ),
  foldEqual(
    ~title="toOption",
    ~expectation="when singleton => Some",
    ~a=() => S.singleton(1)->S.toOption->Option.getWithDefault(S.empty)->S.toArray,
    ~b=[1],
  ),
  foldEqual(
    ~title="toOption",
    ~expectation="when many items => Some",
    ~a=() => oneTwoThree->S.toOption->Option.getWithDefault(S.empty)->S.toArray,
    ~b=[1, 2, 3],
  ),
]

let reduceTests = {
  let add = (total, x) => total + x
  let lastSeen = (_: option<'a>, x: 'a) => Some(x)
  let oneUpTo = n => S.range(~start=1, ~end=n)
  [
    (() => S.empty->S.reduce(-1, add), -1),
    (() => S.singleton(99)->S.reduce(1, add), 100),
    (() => oneTwoThree->S.reduce(1, add), 7),
    (() => oneUpTo(99)->S.reduce(None, lastSeen)->Option.getWithDefault(-1), 99),
    (() => oneUpTo(9999)->S.reduce(None, lastSeen)->Option.getWithDefault(-1), 9999),
    (() => oneUpTo(999_999)->S.reduce(None, lastSeen)->Option.getWithDefault(-1), 999_999),
  ]->Js.Array2.mapi(((a, b), index) =>
    foldEqual(~title="reduce", ~expectation=`index ${index->intToString}`, ~a, ~b)
  )
}

let lastTests = {
  [
    (S.empty, None),
    (1->S.singleton, Some(1)),
    (S.range(~start=1, ~end=9), Some(9)),
    (S.range(~start=1, ~end=99), Some(99)),
    (S.range(~start=1, ~end=999), Some(999)),
    (S.range(~start=1, ~end=999999), Some(999999)),
  ]->Js.Array2.mapi(((xs, result), index) =>
    foldEqual(
      ~title="last",
      ~expectation=`index ${index->intToString}`,
      ~a=() => xs->S.last,
      ~b=result,
    )
  )
}

let (allOkTests, allSomeTests) = {
  let validationTests = [
    ("when empty, return empty", [], Ok([])),
    ("when one error, return it", [30], Error("30")),
    ("when one ok, return it", [2], Ok([4])),
    ("when all ok, return all", [1, 2, 3], Ok([2, 4, 6])),
    ("when all error, return first", [20, 30, 40], Error("20")),
    ("when mix, return first error", [1, 2, 14, 3, 4], Error("14")),
  ]

  let validate = n => n < 10 ? Ok(n * 2) : Error(n->intToString)

  let allOkTests =
    validationTests->Belt.Array.map(((expectation, input, expected)) =>
      foldEqual(
        ~title="allOk",
        ~expectation,
        ~a=() => input->S.fromArray->S.map(validate)->S.allOk->Result.map(i => i->S.toArray),
        ~b=expected,
      )
    )

  let allSomeTests = {
    validationTests->Belt.Array.map(((expectation, input, expected)) =>
      foldEqual(
        ~title="allSome",
        ~expectation,
        ~a=() =>
          input
          ->S.fromArray
          ->S.map(validate)
          ->S.map(Ex.Result.toOption)
          ->S.allSome
          ->Option.map(i => i->S.toArray),
        ~b=expected->Ex.Result.toOption,
      )
    )
  }
  (allOkTests, allSomeTests)
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

let findTests = [
  (() => S.empty, _ => true, None, "when empty and predicate true"),
  (() => S.empty, _ => false, None, "when empty and predicate false"),
  (() => 1->S.singleton, i => i == 1, Some(1), "when singleton and found"),
  (() => 1->S.singleton, _ => false, None, "when singleton and predicate false"),
  (() => [1, 2, 3]->S.fromArray, i => i == 1, Some(1), "when many and is first"),
  (() => [1, 2, 3]->S.fromArray, i => i == 2, Some(2), "when many and is middle"),
  (() => [1, 2, 3]->S.fromArray, i => i == 3, Some(3), "when many and is last"),
  (() => [1, 2, 3]->S.fromArray, _ => false, None, "when many and predicate false"),
  (() => S.range(~start=1, ~end=999_999), i => i == 999_999, Some(999_999), "when million"),
  (() => S.range(~start=1, ~end=999_999), _ => false, None, "when million"),
]->Js.Array2.mapi(((source, predicate, result, note), index) =>
  T.make(
    ~category="Seq",
    ~title="find",
    ~expectation=`${index->intToString} ${note}`,
    ~predicate=() => {
      source()->S.find(predicate) == result
    },
  )
)

let map2Tests = {
  let test = (xs, ys, expected) => {
    T.make(
      ~category="Seq",
      ~title="map2",
      ~expectation=`${xs},${ys} => ${expected}`,
      ~predicate=() => {
        let xs = xs->S.fromString
        let ys = ys->S.fromString
        let expected = expected == "" ? S.empty : expected->Js.String2.split(",")->S.fromArray
        let actual = S.map2(xs, ys, (x, y) => x ++ y)
        S.equals(expected, actual, (i, j) => i == j)
      },
    )
  }
  [
    test("", "", ""),
    test("a", "", ""),
    test("", "a", ""),
    test("a", "b", "ab"),
    test("aa", "bb", "ab,ab"),
    test("aaa", "bbb", "ab,ab,ab"),
    test("a", "bbb", "ab"),
    test("aaa", "b", "ab"),
    test("aaaaaaa", "bbb", "ab,ab,ab"),
  ]
}

let map3Tests = {
  let test = (xs, ys, zs, expected) => {
    T.make(
      ~category="Seq",
      ~title="map3",
      ~expectation=`${xs},${ys},${zs} => ${expected}`,
      ~predicate=() => {
        let xs = xs->S.fromString
        let ys = ys->S.fromString
        let zs = zs->S.fromString
        let expected = expected == "" ? S.empty : expected->Js.String2.split(",")->S.fromArray
        let actual = S.map3(xs, ys, zs, (x, y, z) => x ++ y ++ z)
        S.equals(expected, actual, (i, j) => i == j)
      },
    )
  }
  [
    test("", "", "", ""),
    test("a", "", "", ""),
    test("", "b", "", ""),
    test("", "", "c", ""),
    test("a", "", "c", ""),
    test("a", "b", "c", "abc"),
    test("a", "bb", "ccc", "abc"),
    test("aa", "bb", "ccc", "abc,abc"),
    test("aaaaaaaa", "bbb", "c", "abc"),
    test("aaaaaaaa", "bbbbbb", "ccc", "abc,abc,abc"),
  ]
}

let tests =
  [
    allOkTests,
    allPairsTests,
    allSomeTests,
    basicConstructorTests,
    chunkBySizeTests,
    compareTests,
    concatTests,
    cycleTests,
    dropTests,
    dropUntilTests,
    dropWhileTests,
    equalsTests,
    everyTests,
    filterMapTests,
    filterOkTests,
    filterSomeTests,
    filterTests,
    findMapTests,
    findMapTests,
    findTests,
    findTests,
    flatMapTests,
    flattenTests,
    forEachTests,
    fromArrayTests,
    fromListTests,
    headTailTests,
    headTests,
    indexedTests,
    infiniteTests,
    initTests,
    interleaveManyTests,
    interleaveTests,
    intersperseTests,
    isEqualTests,
    isSortedByTests,
    iterateTests,
    lastTests,
    lengthTests,
    mapTests,
    map2Tests,
    map3Tests,
    memoizeTests,
    minByMaxByTests,
    pairwiseTests,
    prependTests,
    rangeTests,
    reduceTests,
    replicateTests,
    scanTests,
    someTests,
    sortedMergeTests,
    takeAtMostTests,
    takeUntilTests,
    takeWhileTests,
    tapTests,
    toArrayTests,
    toExactlyOneTests,
    toOptionTests,
    toStringTests,
    unfoldTests,
    windowAheadTests,
    windowBehindTests,
    windowTests,
  ]->Belt.Array.flatMap(i => i)
