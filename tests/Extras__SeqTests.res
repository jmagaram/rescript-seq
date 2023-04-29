module T = Extras__Test
module S = Extras__Seq
module R = Extras__Result
module Ex = Extras
module Option = Belt.Option
module Result = Belt.Result
module String = Js.String2

// =============================
// Utilities to help write tests
// =============================

let intToString = Belt.Int.toString

let intCmp = Ex.Cmp.int
let intOptionCmp = (a, b) => Belt.Option.cmp(a, b, intCmp)
let stringCmp = (a: string, b: string) => a < b ? -1 : a > b ? 1 : 0

let characters = s => s->Js.String2.split("")->S.fromArray

let joinInts = xs => xs->Js.Array2.map(intToString)->Js.Array2.joinWith("")

let shorten = s => Js.String2.slice(s, ~from=0, ~to_=1000)

let trueAlways = _ => true
let falseAlways = _ => false

/**
Generates a function that when called returns the number of times it has been invoked.
*/
let callCount = () => {
  let count = ref(0)
  () => {
    count := count.contents + 1
    count.contents
  }
}

/**
Constructs an infinite sequence that returns the number of times it has been
invoked. Useful for tracking that various sequences are completely lazy. For
example, if you use this sequence and do a `take(3)` then it shouldn't be
invoked more than 3 times. Also this is useful when testing sequences that rely
on persistent values. For example, the `allPairs` function should cache the
returned values before creating the pairs.
*/
let callCountForever = () => {
  let f = callCount()
  S.foreverWith(f)
}

/**
`throwIfInvoked` is a test helper function that throw if it is ever called.
*/
let throwIfInvoked = () =>
  Js.Exn.raiseError("Tried to invoke a testing function that always fails.")

/**
`death` defines a sequence that throws an exception if it is ever iterated.
Useful to test that sequence functions are completely lazy.
*/
let death = () => S.onceWith(throwIfInvoked)

/**
`toDispenser(source)` constructs a function from a sequence that returns each
value in that sequence in turn, each time it is called. If any attempt is made
to consume a value once the sequence is complete, an exception is thrown.
*/
let toDispenser = xx => {
  let xx = ref(xx)
  () => {
    switch xx.contents->S.headTail {
    | None => Js.Exn.raiseError("You are attempting to consume a value past the end of a sequence.")
    | Some(x, xx') => {
        xx := xx'
        x
      }
    }
  }
}

/**
`countdown(start)` is a function that returns `start` the first time it is
called, and returns `1` less each subsequent time. If the function is called
after the countdown has completed - meaning the value has reached `0` - an exception is thrown. This is useful for testing to ensure a callback is
  never called more than necessary.
*/
let countdown = n => {
  if n < 0 {
    S.InvalidArgument("The countdown must begin at 0 or more.")->raise
  }
  let count = ref(n)
  () => {
    if count.contents == 0 {
      Js.Exn.raiseError(
        "Attempted to consume a value from the countdown after it had been exhausted.",
      )
    }
    count := count.contents - 1
    count.contents + 1
  }
}

/**
Create a test that compares two sequences for equality. Converts both sequences
to arrays and then uses the ReScript recursive equality test.
*/
let seqEqual = (~title, ~expectation, ~a, ~b) =>
  T.fromResult(~category="Seq", ~title, ~expectation, () => {
    let a = a()->S.toArray
    switch a == b {
    | true => Ok()
    | false =>
      Error(
        `Actual:   ${a->Js.Array2.toString->shorten}\nExpected: ${b->Js.Array2.toString->shorten}`,
      )
    }
  })

/**
Creates a test that compares two values for equality. Uses the ReScript
recursive equality test.
*/
let valueEqual = (~title, ~expectation, ~a, ~b) =>
  T.fromResult(~category="Seq", ~title, ~expectation, () => {
    let aValue = a()
    switch aValue == b {
    | true => Ok()
    | false => Error(`Actual:   ${aValue->Obj.magic}\nExpected: ${b->Obj.magic}`)
    }
  })

/**
Creates a test that passes if the provided function throws any kind of
exception.
*/
let willThrow = (~title, ~expectation=?, f) =>
  T.expectThrow(~category="Seq", ~title, ~expectation?, f)

/**
Creates a test that passes if the provided function does NOT throw an exception.
*/
let willNotThrow = (~title, ~expectation, f) =>
  T.fromPredicate(~category="Seq", ~title, ~expectation, () =>
    Ex.Result.fromTryCatch(f)->Result.isOk
  )

/**
Makes a series of sequence equal tests when fed an array of tuples. The tuple
parameters are (1) a sequence, (2) an array representing the expected values in
the sequence, and (3) a test note. All tuples and sequences must have the same
type. Sequences are converted to arrays and then the ReScript array comparision
function is used.
*/
let makeSeqEqualsTests = (~title, xs) =>
  xs->Js.Array2.mapi(((source, result, note), inx) =>
    seqEqual(~title, ~expectation=`index ${inx->intToString} ${note}`, ~a=() => source, ~b=result)
  )

/**
Makes a series of value equal tests when fed an array of tuples. The tuple
parameters are (1) a lazy value, (2) the expected value, and (3) a note. Uses
the ReScript recursive equality test.
*/
let makeValueEqualTests = (~title, tests) =>
  tests->Js.Array2.mapi(((lazyA, b, note), index) =>
    valueEqual(~title, ~a=lazyA, ~b, ~expectation=`Index ${index->Belt.Int.toString} ${note}`)
  )

// =============================================================================
// The tests. Use one let statement to make an array of tests for function being
// tested. Then add that array of values to the collection at the end of this
// file.
// =============================================================================

let onceTests = [
  seqEqual(~title="once", ~expectation="string", ~a=() => S.once("x"), ~b=["x"]),
  seqEqual(~title="once", ~expectation="bool", ~a=() => S.once(true), ~b=[true]),
  seqEqual(~title="once", ~expectation="int", ~a=() => S.once(1), ~b=[1]),
  seqEqual(~title="once", ~expectation="object", ~a=() => S.once({"a": 1}), ~b=[{"a": 1}]),
  seqEqual(~title="once", ~expectation="array", ~a=() => S.once([1, 2, 3]), ~b=[[1, 2, 3]]),
]

let onceWithTests = [
  seqEqual(~title="onceWith", ~expectation="string", ~a=() => S.onceWith(() => "x"), ~b=["x"]),
  seqEqual(~title="onceWith", ~expectation="bool", ~a=() => S.onceWith(() => true), ~b=[true]),
  seqEqual(~title="onceWith", ~expectation="object", ~a=() => S.once({"a": 1}), ~b=[{"a": 1}]),
  seqEqual(~title="onceWith", ~expectation="array", ~a=() => S.onceWith(() => [1, 2]), ~b=[[1, 2]]),
  valueEqual(
    ~title="onceWith",
    ~expectation="generator function only called when enumerated",
    ~a=() => {
      S.onceWith(throwIfInvoked)->ignore
      1
    },
    ~b=1,
  ),
  seqEqual(
    ~title="onceWith",
    ~expectation="generator function called one time at most",
    ~a=() => S.onceWith(countdown(1)),
    ~b=[1],
  ),
]

let emptyTests = [seqEqual(~title="empty", ~expectation="has no items", ~a=() => S.empty, ~b=[])]

let consTests = [
  seqEqual(~title="cons", ~expectation="when a + empty => a", ~a=() => S.cons(1, S.empty), ~b=[1]),
  seqEqual(
    ~title="cons",
    ~expectation="when a + bcd => abcd",
    ~a=() => S.cons(1, S.range(2, 4)),
    ~b=[1, 2, 3, 4],
  ),
]

let fromOptionTests =
  makeSeqEqualsTests(
    ~title="fromOption",
    [
      (None->S.fromOption, [], "if none => empty"),
      (Some(1)->S.fromOption, [1], "if some => sequence with one item"),
    ],
  )->Js.Array2.concat([
    seqEqual(
      ~title="fromOption",
      ~expectation="array is not flattened",
      ~a=() => Some([1, 2, 3])->S.fromOption,
      ~b=[[1, 2, 3]],
    ),
  ])

let fromListTests = makeSeqEqualsTests(
  ~title="fromList",
  [
    (list{1, 2, 3, 4, 5}->S.fromList, [1, 2, 3, 4, 5], "many"),
    (list{1}->S.fromList, [1], "one item"),
    (list{}->S.fromList, [], "empty"),
  ],
)

let cycleTests = makeSeqEqualsTests(
  ~title="cycle",
  [
    (S.empty->S.cycle, [], "when empty => empty"),
    (S.once(1)->S.cycle->S.take(4), [1, 1, 1, 1], ""),
    ([1, 2, 3]->S.fromArray->S.cycle->S.take(9), [1, 2, 3, 1, 2, 3, 1, 2, 3], ""),
    (S.forever(1)->S.cycle->S.take(4), [1, 1, 1, 1], "when infinite can still cycle"),
    (
      callCountForever()->S.take(3)->S.cycle->S.take(9),
      [1, 2, 3, 4, 5, 6, 7, 8, 9],
      "the process to generate items is cycled, not the items themselves.",
    ),
    (
      callCountForever()->S.take(3)->S.cache->S.cycle->S.take(6),
      [1, 2, 3, 1, 2, 3],
      "can use cache to cycle values, not the process.",
    ),
  ],
)->Js.Array2.concat([
  valueEqual(
    ~title="cycle",
    ~expectation="empty determination is lazy",
    ~a=() => {
      S.foreverWith(throwIfInvoked)->S.tap(_ => Js.log("hmm..."))->S.cycle->ignore
      1
    },
    ~b=1,
  ),
  valueEqual(
    ~title="cycle",
    ~expectation="millions",
    ~a=() => S.range(1, 1000)->S.cycle->S.take(1000 * 1000)->S.last,
    ~b=Some(1000),
  ),
])

let allPairsTests = makeSeqEqualsTests(
  ~title="allPairs",
  [
    (S.allPairs(S.empty, S.empty), [], ""),
    (S.allPairs(S.empty, S.range(4, 6)), [], ""),
    (S.allPairs(S.once(1), S.once(2)), [(1, 2)], ""),
    (S.allPairs(S.once(1), S.range(4, 6)), [(1, 4), (1, 5), (1, 6)], ""),
    (S.allPairs(S.range(4, 6), S.once(1)), [(4, 1), (5, 1), (6, 1)], ""),
    (
      S.allPairs(S.range(1, 3), S.range(4, 6)),
      [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)],
      "",
    ),
    (
      S.allPairs(S.replicateWith(2, countdown(2)), S.replicateWith(2, countdown(2))),
      [(2, 2), (2, 1), (1, 2), (1, 1)],
      "cached!",
    ),
  ],
)

let rangeTests = makeSeqEqualsTests(
  ~title="range",
  [
    (S.range(1, 1), [1], ""),
    (S.range(1, 2), [1, 2], ""),
    (S.range(4, 6), [4, 5, 6], ""),
    (S.range(6, 1), [6, 5, 4, 3, 2, 1], ""),
    (S.range(-3, 3), [-3, -2, -1, 0, 1, 2, 3], ""),
  ],
)

let rangeMapTests = makeSeqEqualsTests(
  ~title="rangeMap",
  [
    (S.rangeMap(1, 1, intToString), ["1"], ""),
    (S.rangeMap(1, 2, intToString), ["1", "2"], ""),
    (S.rangeMap(4, 6, intToString), ["4", "5", "6"], ""),
    (S.rangeMap(6, 1, intToString), ["6", "5", "4", "3", "2", "1"], ""),
    (S.rangeMap(-3, 3, intToString), ["-3", "-2", "-1", "0", "1", "2", "3"], ""),
  ],
)

let concatTests = makeSeqEqualsTests(
  ~title="concat",
  [
    (S.concat(S.range(1, 3), S.range(4, 6)), [1, 2, 3, 4, 5, 6], ""),
    (S.concat(S.empty, S.range(1, 3)), [1, 2, 3], ""),
    (S.concat(S.range(1, 3), S.empty), [1, 2, 3], ""),
    (S.concat(S.empty, S.empty), [], ""),
  ],
)

let prependTests = makeSeqEqualsTests(
  ~title="prepend",
  [
    (S.prepend(S.range(1, 3), S.range(4, 6)), [4, 5, 6, 1, 2, 3], ""),
    (S.prepend(S.empty, S.range(1, 3)), [1, 2, 3], ""),
    (S.prepend(S.range(1, 3), S.empty), [1, 2, 3], ""),
    (S.prepend(S.empty, S.empty), [], ""),
  ],
)

let flatMapTests =
  makeSeqEqualsTests(
    ~title="flatMap",
    [
      (S.empty->S.flatMap(i => S.replicate(i, i)), [], ""),
      (S.once(2)->S.flatMap(i => S.replicate(i, 6)), [6, 6], ""),
      (S.once(2)->S.flatMap(_ => S.empty), [], ""),
      (S.range(1, 3)->S.flatMap(i => S.replicate(i, i)), [1, 2, 2, 3, 3, 3], ""),
      (S.range(1, 3)->S.flatMap(_ => S.empty), [], ""),
      (S.range(1, 3)->S.flatMap(i => S.once(i)), [1, 2, 3], ""),
    ],
  )->Js.Array2.concat([
    valueEqual(
      ~title="flatMap",
      ~expectation="million won't overflow",
      ~a=() =>
        S.replicate(1000, 1)->S.flatMap(_ => S.replicate(1000, 1))->S.concat(S.once(999))->S.last,
      ~b=Some(999),
    ),
  ])

let mapTests = makeSeqEqualsTests(
  ~title="map",
  [
    (S.range(1, 5)->S.map(i => i + 1), [2, 3, 4, 5, 6], ""),
    (S.once(1)->S.map(i => i + 1), [2], ""),
    (S.empty->S.map(i => i + 1), [], ""),
    (S.range(1, 3)->S.mapi((n, inx) => n * inx), [0, 2, 6], ""),
  ],
)

let indexedTests = makeSeqEqualsTests(
  ~title="indexed",
  [
    (S.range(1, 3)->S.indexed, [(1, 0), (2, 1), (3, 2)], ""),
    (S.once(9)->S.indexed, [(9, 0)], ""),
    (S.empty->S.indexed, [], ""),
  ],
)

let takeWhileTests = makeSeqEqualsTests(
  ~title="takeWhile",
  [
    (S.range(1, 5)->S.takeWhile(i => i <= 3), [1, 2, 3], ""),
    (S.range(1, 5)->S.takeWhile(i => i == 1), [1], ""),
    (S.range(1, 5)->S.takeWhile(_ => false), [], ""),
    (S.range(1, 5)->S.takeWhile(i => i <= 5), [1, 2, 3, 4, 5], ""),
    (S.empty->S.takeWhile(_ => true), [], ""),
    (S.range(1, 5)->S.takeWhile(_ => false), [], ""),
  ],
)

let dropTests = makeSeqEqualsTests(
  ~title="drop",
  [
    (S.range(1, 3)->S.drop(0), [1, 2, 3], ""),
    (S.range(1, 3)->S.drop(1), [2, 3], ""),
    (S.range(1, 3)->S.drop(3), [], ""),
    (S.range(1, 3)->S.drop(4), [], ""),
    (S.empty->S.drop(0), [], ""),
    (S.empty->S.drop(1), [], ""),
    (S.once(4)->S.drop(0), [4], ""),
    (S.once(4)->S.drop(1), [], ""),
  ],
)->Js.Array2.concat([
  willNotThrow(~title="drop", ~expectation="lazy", () => death()->S.drop(1)),
  valueEqual(
    ~title="drop",
    ~expectation="if drop 0, return same seq instance",
    ~a=() => {
      let oneToFive = S.range(1, 5)
      oneToFive->S.drop(0) === oneToFive
    },
    ~b=true,
  ),
  valueEqual(
    ~title="drop",
    ~expectation="while drop a million, no overflow",
    ~a=() => S.range(1, 999_999)->S.concat(1_000_000->S.once)->S.drop(999_999)->S.last,
    ~b=Some(1_000_000),
  ),
])

let flattenTests = makeSeqEqualsTests(
  ~title="flatten",
  [
    (S.once(S.empty)->S.flatten, [], ""),
    (S.range(1, 3)->S.map(i => S.replicate(i, i))->S.flatten, [1, 2, 2, 3, 3, 3], ""),
    (S.empty->S.flatten, [], ""),
  ],
)

let sortedMergeTests = {
  let merge = (nums1, nums2, nums3) =>
    seqEqual(
      ~title="sortedMerge",
      ~expectation="",
      ~a=() => S.sortedMerge(nums1->S.fromArray, nums2->S.fromArray, intCmp),
      ~b=nums3,
    )
  [
    merge([], [], []),
    merge([], [1, 2, 3], [1, 2, 3]),
    merge([1, 2, 3], [], [1, 2, 3]),
    merge([1, 2, 3], [1, 2, 3], [1, 1, 2, 2, 3, 3]),
    merge([4, 5, 6], [1, 2, 3], [1, 2, 3, 4, 5, 6]),
    merge([1, 1, 3, 6, 6, 8, 8, 9], [0, 2, 2, 7, 9], [0, 1, 1, 2, 2, 3, 6, 6, 7, 8, 8, 9, 9]),
  ]
}

let tapTests = [
  valueEqual(
    ~title="tap",
    ~expectation="can inspect every value",
    ~a=() => {
      let seen = []
      S.range(1, 5)->S.tap(i => seen->Js.Array2.push(i)->ignore)->S.consume
      seen
    },
    ~b=[1, 2, 3, 4, 5],
  ),
  willNotThrow(~title="tap", ~expectation="lazy", () => death()->S.tap(_ => Js.log("Boom!"))),
]

let windowTests =
  makeSeqEqualsTests(
    ~title="window",
    [
      (S.range(1, 5)->S.window(3)->S.map(joinInts), ["123", "234", "345"], ""),
      (S.range(1, 5)->S.window(2)->S.map(joinInts), ["12", "23", "34", "45"], ""),
      (S.range(1, 5)->S.window(1)->S.map(joinInts), ["1", "2", "3", "4", "5"], ""),
      (S.range(1, 5)->S.window(999_999)->S.map(joinInts), [], ""),
      (S.range(1, 5)->S.take(0)->S.window(1)->S.map(joinInts), [], ""),
    ],
  )->Js.Array2.concat([
    willNotThrow(~title="window", ~expectation="lazy", () => death()->S.window(5)),
    willThrow(~title="window", ~expectation="when size = 0 => throw", () =>
      S.range(1, 5)->S.window(0)->S.consume
    ),
    willThrow(~title="window", ~expectation="when size < 0 => throw", () =>
      S.range(1, 5)->S.window(-1)->S.consume
    ),
  ])

let windowAheadBehindTests = (~title, ~function, ~data) =>
  {
    let strOrEmpty = s => s == "" ? "(empty)" : s
    let oneTest = (input, size, expectedResult) => {
      valueEqual(
        ~title,
        ~expectation=`${strOrEmpty(input)} size ${size->intToString} => ${strOrEmpty(
            expectedResult,
          )}`,
        ~a=() =>
          input
          ->characters
          ->function(size)
          ->S.map(ss => ss->Js.Array2.joinWith(""))
          ->S.intersperse(",")
          ->S.join(""),
        ~b=expectedResult,
      )
    }
    data->Js.Array2.map(((input, size, expected)) => oneTest(input, size, expected))
  }->Js.Array2.concat([
    willThrow(~title, ~expectation="when size == 0 => throw", () => "abc"->characters->function(0)),
    willThrow(~title, ~expectation="when size < 0 => throw", () => "abc"->characters->function(-1)),
    willNotThrow(~title, ~expectation="lazy", () => death()->function(1)),
  ])

let windowAheadTests = windowAheadBehindTests(
  ~title="windowAhead",
  ~function=S.windowAhead,
  ~data=[
    ("", 1, ""),
    ("", 2, ""),
    ("a", 1, "a"),
    ("a", 2, "a"),
    ("ab", 1, "a,b"),
    ("ab", 2, "ab,b"),
    ("abc", 2, "ab,bc,c"),
    ("abc", 3, "abc,bc,c"),
    ("abc", 4, "abc,bc,c"),
    ("abcde", 1, "a,b,c,d,e"),
    ("abcdefg", 3, "abc,bcd,cde,def,efg,fg,g"),
    ("abcdefg", 7, "abcdefg,bcdefg,cdefg,defg,efg,fg,g"),
  ],
)

let windowBehindTests = windowAheadBehindTests(
  ~title="windowBehind",
  ~function=S.windowBehind,
  ~data=[
    ("", 1, ""),
    ("", 2, ""),
    ("a", 1, "a"),
    ("a", 2, "a"),
    ("ab", 1, "a,b"),
    ("ab", 2, "a,ab"),
    ("abc", 2, "a,ab,bc"),
    ("abc", 3, "a,ab,abc"),
    ("abc", 4, "a,ab,abc"),
    ("abcde", 1, "a,b,c,d,e"),
    ("abcdefg", 3, "a,ab,abc,bcd,cde,def,efg"),
    ("abcdefg", 7, "a,ab,abc,abcd,abcde,abcdef,abcdefg"),
  ],
)

let pairwiseTests =
  makeSeqEqualsTests(
    ~title="pairwise",
    [
      (S.empty->S.pairwise, [], ""),
      (S.once(1)->S.pairwise, [], ""),
      (S.range(1, 2)->S.pairwise, [(1, 2)], ""),
      (S.range(1, 5)->S.pairwise, [(1, 2), (2, 3), (3, 4), (4, 5)], ""),
    ],
  )->Js.Array2.concat([
    willNotThrow(~title="pairwise", ~expectation="lazy", () => death()->S.pairwise),
  ])

let interleaveTests = makeSeqEqualsTests(
  ~title="interleave",
  [
    (S.interleave(S.empty, S.empty), [], ""),
    (S.interleave(S.range(1, 3), S.range(4, 6)), [1, 4, 2, 5, 3, 6], ""),
    (S.interleave(S.range(4, 7), S.once(1)), [4, 1, 5, 6, 7], ""),
    (S.interleave(S.once(1), S.range(4, 7)), [1, 4, 5, 6, 7], ""),
    (S.interleave(S.range(1, 3), S.range(4, 6)), [1, 4, 2, 5, 3, 6], ""),
  ],
)

let iterateTests = makeSeqEqualsTests(
  ~title="iterate",
  [
    (S.iterate(2, i => i * 2)->S.take(3), [2, 4, 8], ""),
    (S.iterate(2, i => i * 2)->S.take(1), [2], ""),
    (S.iterate(2, i => i * 2)->S.take(0), [], ""),
    (
      S.iterate(1, i => i + 1)->S.take(999_999)->S.filter(i => i === 999_999),
      [999_999],
      "millions",
    ),
  ],
)->Js.Array2.concat([
  willNotThrow(~title="iterate", ~expectation="lazy", () =>
    S.iterate(0, i => {
      Js.Exn.raiseError("Boom!")->raise->ignore
      i
    })
  ),
])

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
  let throws = f => willThrow(~title="fromArray", ~expectation="throws", f)
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
    (S.replicate(0, "x"), [], ""),
    (S.replicate(1, "x"), ["x"], "x"),
    (S.replicate(2, "x"), ["x", "x"], ""),
    (S.replicate(3, "x"), ["x", "x", "x"], ""),
  ],
)

let replicateWithTests = makeSeqEqualsTests(
  ~title="replicateWith",
  [
    (S.replicateWith(0, () => "x"), [], ""),
    (S.replicateWith(1, () => "x"), ["x"], "x"),
    (S.replicateWith(2, () => "x"), ["x", "x"], ""),
    (S.replicateWith(3, () => "x"), ["x", "x", "x"], ""),
  ],
)

let foreverTests = [
  valueEqual(
    ~title="forever",
    ~expectation="millions",
    ~a=() => S.forever("x")->S.indexed->S.takeUntil(((_, inx)) => inx == 999_999)->S.last,
    ~b=Some(("x", 999_999)),
  ),
]

let takeTests = makeSeqEqualsTests(
  ~title="take",
  [
    (S.empty->S.take(0), [], ""),
    (S.empty->S.take(1), [], ""),
    (S.once(1)->S.take(0), [], ""),
    (S.once(1)->S.take(1), [1], ""),
    (S.once(1)->S.take(2), [1], ""),
    (S.range(1, 5)->S.take(0), [], ""),
    (S.range(1, 5)->S.take(1), [1], ""),
    (S.range(1, 5)->S.take(3), [1, 2, 3], ""),
    (S.range(1, 5)->S.take(5), [1, 2, 3, 4, 5], ""),
    (S.range(1, 5)->S.take(6), [1, 2, 3, 4, 5], ""),
  ],
)->Js.Array2.concat([
  valueEqual(
    ~title="take",
    ~expectation="millions",
    ~a=() => S.range(1, 999_999)->S.last,
    ~b=Some(999_999),
  ),
  willNotThrow(~title="take", ~expectation="lazy", () => death()->S.take(99)),
  willNotThrow(~title="take", ~expectation="if zero, do not iterate anything", () =>
    death()->S.take(0)->S.consume
  ),
  valueEqual(
    ~title="take",
    ~expectation="if take 999_999, generator function called 999_999 times",
    ~a=() => {
      let callCount = ref(0)
      S.foreverWith(() => {
        callCount := callCount.contents + 1
        callCount.contents
      })
      ->S.take(999_999)
      ->S.consume
      callCount.contents
    },
    ~b=999_999,
  ),
])

let foreverWithTests =
  makeSeqEqualsTests(
    ~title="foreverWith",
    [
      (callCountForever()->S.take(0), [], ""),
      (callCountForever()->S.take(1), [1], ""),
      (callCountForever()->S.take(5), [1, 2, 3, 4, 5], ""),
      (
        callCountForever()
        ->S.take(999_999)
        ->S.last
        ->Option.map(S.once)
        ->Option.getWithDefault(S.empty),
        [999_999],
        "",
      ),
    ],
  )->Js.Array2.concat([
    willNotThrow(~title="foreverWith", ~expectation="lazy", () => S.foreverWith(throwIfInvoked)),
  ])

let unfoldTests =
  makeSeqEqualsTests(
    ~title="unfold",
    [
      (S.unfold(1, i => i <= 5 ? Some(i, i + 1) : None), [1, 2, 3, 4, 5], ""),
      (S.unfold(1, _ => None), [], "zero items"),
      (S.unfold(1, i => i < 100 ? Some(i, i * 2) : None), [1, 2, 4, 8, 16, 32, 64], ""),
      (
        S.unfold((0, 1), ((a, b)) => a + b <= 100 ? Some(a + b, (b, a + b)) : None)->S.prepend(
          [0, 1]->S.fromArray,
        ),
        [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89],
        "fibonacci",
      ),
    ],
  )->Js.Array2.concat([
    valueEqual(
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
      (S.init(1, inx => inx * 2), [0], ""),
      (S.init(2, inx => inx * 2), [0, 2], ""),
      (S.init(3, inx => inx * 2), [0, 2, 4], ""),
    ],
  )->Js.Array2.concat([
    valueEqual(
      ~title="init",
      ~expectation="tens",
      ~a=() => S.init(100, inx => inx)->S.last,
      ~b=Some(99),
    ),
    valueEqual(
      ~title="init",
      ~expectation="millions",
      ~a=() => S.range(0, 999_999)->S.last,
      ~b=Some(999_999),
    ),
  ])

let chunkBySizeTests = {
  let process = (xs, n) => xs->S.chunkBySize(n)->S.map(joinInts)
  makeSeqEqualsTests(
    ~title="chunkBySize",
    [
      (S.empty->process(1), [], ""),
      (S.once(1)->process(1), ["1"], ""),
      (S.once(1)->process(2), ["1"], ""),
      (S.range(1, 3)->process(1), ["1", "2", "3"], ""),
      (S.range(1, 3)->process(2), ["12", "3"], ""),
      (S.range(1, 3)->process(3), ["123"], ""),
      (S.range(1, 3)->process(4), ["123"], "millions"),
      (
        S.range(0, 9)
        ->S.cycle
        ->S.take(1_000_000)
        ->S.chunkBySize(10)
        ->S.map(joinInts)
        ->S.last
        ->Option.map(S.once)
        ->Option.getWithDefault(""->S.once),
        ["0123456789"],
        "",
      ),
    ],
  )->Js.Array2.concat([
    willThrow(~title="chunkBySize", ~expectation="when size == 0", () =>
      S.range(1, 5)->S.chunkBySize(0)
    ),
    willThrow(~title="chunkBySize", ~expectation="when size == -1", () =>
      S.range(1, 5)->S.chunkBySize(0)
    ),
    willNotThrow(~title="chunkBySize", ~expectation="lazy", () => death()->S.chunkBySize(5)),
  ])
}

let scanTests = {
  let push = (sum, x) => {
    let copied = sum->Js.Array2.copy
    copied->Js.Array2.push(x)->ignore
    copied
  }
  let scanConcat = xs => xs->S.scan([0], push)->S.map(joinInts)
  makeSeqEqualsTests(
    ~title="scan",
    [
      (S.range(1, 5)->scanConcat, ["0", "01", "012", "0123", "01234", "012345"], ""),
      (S.range(1, 2)->scanConcat, ["0", "01", "012"], ""),
      (S.once(1)->scanConcat, ["0", "01"], ""),
      (S.empty->scanConcat, ["0"], "always includes the zero"),
      (
        S.range(1, 999_999)
        ->S.scan(-1, (_, i) => i)
        ->S.map(intToString)
        ->S.last
        ->Option.map(s => S.once(s))
        ->Option.getWithDefault(S.once("")),
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
    (1->S.once->S.takeUntil(i => i == 1), [1], ""),
    (1->S.once->S.takeUntil(falseAlways), [1], ""),
    (1->S.once->S.takeUntil(trueAlways), [1], ""),
    (S.range(1, 5)->S.takeUntil(trueAlways), [1], ""),
    (S.range(1, 5)->S.takeUntil(falseAlways), [1, 2, 3, 4, 5], ""),
    (S.range(1, 5)->S.takeUntil(i => i == 3), [1, 2, 3], ""),
    (S.range(1, 5)->S.takeUntil(i => i == 1), [1], ""),
    (S.range(1, 5)->S.takeUntil(i => i == 5), [1, 2, 3, 4, 5], ""),
    ([1, 2, 2, 2, 3]->S.fromArray->S.takeUntil(i => i == 2), [1, 2], ""),
    (
      S.range(1, 999_999)
      ->S.takeUntil(i => i == 999_999)
      ->S.last
      ->Option.map(S.once)
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

let intersperseWithTests = {
  let f = () => S.range(1, 100)->S.map(intToString)->toDispenser
  makeSeqEqualsTests(
    ~title="intersperseWith",
    [
      ([]->S.fromArray->S.intersperseWith(f()), [], ""),
      (["a"]->S.fromArray->S.intersperseWith(f()), ["a"], ""),
      (["a", "b"]->S.fromArray->S.intersperseWith(f()), ["a", "1", "b"], ""),
      (["a", "b", "c"]->S.fromArray->S.intersperseWith(f()), ["a", "1", "b", "2", "c"], ""),
    ],
  )
}

let dropWhileTests =
  [
    (S.empty, falseAlways, [], ""),
    (S.empty, trueAlways, [], ""),
    (1->S.once, i => i == 1, [], ""),
    (1->S.once, falseAlways, [1], ""),
    (1->S.once, trueAlways, [], ""),
    (S.range(1, 5), trueAlways, [], ""),
    (S.range(1, 5), falseAlways, [1, 2, 3, 4, 5], ""),
    (S.range(1, 5), i => i < 3, [3, 4, 5], ""),
    (S.range(1, 5), i => i <= 5, [], ""),
    (S.range(1, 5), i => i <= 1, [2, 3, 4, 5], ""),
    (S.range(1, 99), i => i != 99, [99], "tens"),
    (S.range(1, 9_999), i => i != 9_999, [9_999], "thousands"),
    (S.range(1, 999_999), i => i != 999_999, [999_999], "millions"),
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
    (S.once(None)->S.filterSome, [], ""),
    (S.once(Some(1))->S.filterSome, [1], ""),
    ([Some(1), None, Some(3), None]->S.fromArray->S.filterSome, [1, 3], ""),
  ],
)

let filterOkTests = makeSeqEqualsTests(
  ~title="filterOk",
  [
    (S.empty->S.filterOk, [], ""),
    (S.once(Error("x"))->S.filterOk, [], ""),
    (S.once(Ok(1))->S.filterOk, [1], ""),
    ([Ok(1), Error("x"), Ok(3), Error("x")]->S.fromArray->S.filterOk, [1, 3], ""),
  ],
)

let filterMapTests =
  [
    (S.empty, _ => Some(1), [], ""),
    (S.empty, _ => None, [], ""),
    (1->S.once, i => Some(i * 2), [2], ""),
    (1->S.once, _ => None, [], ""),
    (S.range(1, 5), i => Some(i), [1, 2, 3, 4, 5], ""),
    (S.range(1, 5), _ => None, [], ""),
    (S.range(1, 5), i => i == 3 || i == 5 ? Some(i) : None, [3, 5], ""),
    (S.range(1, 5), i => i == 5 ? Some(99) : None, [99], ""),
    (S.range(1, 5), i => i == 1 || i == 3 ? Some(i * 2) : None, [2, 6], ""),
    (S.range(1, 999_999), i => i == 999_999 ? Some(-i) : None, [-999_999], "millions"),
  ]
  ->Js.Array2.mapi(((source, f, result, note), inx) =>
    seqEqual(
      ~title="filterMap",
      ~expectation=`index ${inx->intToString} ${note}`,
      ~a=() => source->S.filterMap(f),
      ~b=result,
    )
  )
  ->Js.Array2.concat({
    let items = [Some(Some(1)), None, Some(None), Some(Some(2)), None, Some(None)]
    [
      valueEqual(
        ~title="filterMap",
        ~expectation="option of options yields correct values",
        ~a=() =>
          items
          ->S.fromArray
          ->S.filterMap(i => i)
          ->S.equals([Some(1), None, Some(2), None]->S.fromArray, (a, b) =>
            intOptionCmp(a, b) == 0
          ),
        ~b=true,
      ),
    ]
  })

let filterMapiTests = makeSeqEqualsTests(
  ~title="filterMapi",
  [
    (S.range(1, 3)->S.filterMapi((num, inx) => num == 2 && inx == 1 ? Some(num) : None), [2], ""),
    (S.range(1, 3)->S.filterMapi((_, inx) => Some(inx)), [0, 1, 2], ""),
    (S.empty->S.filterMapi((_, inx) => Some(inx)), [], ""),
    (
      S.range(1, 999_999)->S.filterMapi((num, inx) => num == 999_999 ? Some(inx) : None),
      [999_998],
      "",
    ),
  ],
)->Js.Array2.concat({
  let items = [Some(Some(1)), None, Some(None), Some(Some(2)), None, Some(None)]
  [
    valueEqual(
      ~title="filterMapi",
      ~expectation="option of options yields correct values",
      ~a=() =>
        items
        ->S.fromArray
        ->S.filterMapi((value, _inx) => value)
        ->S.equals([Some(1), None, Some(2), None]->S.fromArray, (a, b) => intOptionCmp(a, b) == 0),
      ~b=true,
    ),
  ]
})

let filterTests =
  [
    (S.empty, falseAlways, [], ""),
    (S.empty, trueAlways, [], ""),
    (1->S.once, i => i == 1, [1], ""),
    (1->S.once, falseAlways, [], ""),
    (1->S.once, trueAlways, [1], ""),
    (S.range(1, 5), trueAlways, [1, 2, 3, 4, 5], ""),
    (S.range(1, 5), falseAlways, [], ""),
    (S.range(1, 5), i => i == 3 || i == 5, [3, 5], ""),
    (S.range(1, 5), i => i == 5, [5], ""),
    (S.range(1, 5), i => i == 1 || i == 3, [1, 3], ""),
    (S.range(1, 999_999), i => i == 999_999, [999_999], "millions"),
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
      ~a=() => S.range(1, 5)->S.filteri((n, inx) => n == 3 && inx == 2),
      ~b=[3],
    ),
    seqEqual(
      ~title="filteri",
      ~expectation="when skipping millions => no stack problem",
      ~a=() => S.replicate(999_999, 1)->S.concat(2->S.once)->S.filteri((value, _) => value != 1),
      ~b=[2],
    ),
  ])

let dropUntilTests =
  [
    (S.empty, falseAlways, [], ""),
    (S.empty, trueAlways, [], ""),
    (1->S.once, i => i == 1, [1], ""),
    (1->S.once, falseAlways, [], ""),
    (1->S.once, trueAlways, [1], ""),
    (S.range(1, 5), trueAlways, [1, 2, 3, 4, 5], ""),
    (S.range(1, 5), falseAlways, [], ""),
    (S.range(1, 5), i => i == 3, [3, 4, 5], ""),
    (S.range(1, 5), i => i == 5, [5], ""),
    (S.range(1, 5), i => i == 1, [1, 2, 3, 4, 5], ""),
    (S.range(1, 999_999), i => i == 999_999, [999_999], "millions"),
  ]->Js.Array2.mapi(((source, predicate, result, note), inx) =>
    seqEqual(
      ~title="dropUntil",
      ~expectation=`index ${inx->intToString} ${note}`,
      ~a=() => source->S.dropUntil(predicate),
      ~b=result,
    )
  )

let forEachTests = [
  valueEqual(
    ~title="forEach",
    ~expectation="every value is enumerated",
    ~a=() => {
      let result = []
      S.range(1, 5)->S.forEach(i => result->Js.Array2.push(i)->ignore)
      result
    },
    ~b=[1, 2, 3, 4, 5],
  ),
  valueEqual(
    ~title="forEach",
    ~expectation="millions",
    ~a=() => {
      let lastSeen = ref(None)
      S.range(1, 999_999)->S.forEach(i => lastSeen := Some(i))
      lastSeen.contents
    },
    ~b=Some(999_999),
  ),
  valueEqual(
    ~title="forEach",
    ~expectation="empty",
    ~a=() => {
      let lastSeen = ref(None)
      S.empty->S.forEach(i => lastSeen := Some(i))
      lastSeen.contents
    },
    ~b=None,
  ),
  valueEqual(
    ~title="forEachi",
    ~expectation="",
    ~a=() => {
      let result = []
      S.range(1, 5)->S.forEachi((n, inx) => result->Js.Array2.push((n, inx))->ignore)
      result
    },
    ~b=[(1, 0), (2, 1), (3, 2), (4, 3), (5, 4)],
  ),
]

let someTests = makeValueEqualTests(
  ~title="some",
  [
    (() => S.empty->S.some(_ => true), false, "if empty is false"),
    (() => S.empty->S.some(_ => false), false, "if empty is false"),
    (() => S.range(1, 3)->S.some(i => i == 2), true, ""),
    (() => S.range(1, 3)->S.some(_ => false), false, ""),
    (() => S.range(1, 999_999)->S.some(i => i == 999_999), true, "millions"),
  ],
)

let everyTests = makeValueEqualTests(
  ~title="every",
  [
    (() => S.empty->S.every(_ => true), true, "if empty is true"),
    (() => S.empty->S.every(_ => false), true, "if empty is true"),
    (() => S.range(1, 3)->S.every(i => i >= 1), true, ""),
    (() => S.range(1, 3)->S.every(_ => false), false, ""),
    (() => S.range(1, 999_999)->S.every(_ => true), true, "millions"),
  ],
)

let findMapTests = makeValueEqualTests(
  ~title="findMap",
  [
    (() => S.range(1, 5)->S.findMap(i => i == 2 ? Some("x") : None), Some("x"), "if found, some"),
    (() => S.range(1, 5)->S.findMap(_ => None), None, "if not found, none"),
    (() => S.range(1, 99_999)->S.findMap(i => i == 99_999 ? Some("x") : None), Some("x"), "big"),
  ],
)->Js.Array2.concat([
  valueEqual(
    ~title="findMap",
    ~expectation="nested options - find Some(Some(_))",
    ~a=() => [None, Some(Some(1)), Some(None)]->S.fromArray->S.findMap(i => i)->Option.getExn,
    ~b=Some(1),
  ),
  valueEqual(
    ~title="findMap",
    ~expectation="nested options - find Some(None)",
    ~a=() =>
      [Some(None), None, Some(Some(1)), Some(None)]->S.fromArray->S.findMap(i => i)->Option.getExn,
    ~b=None,
  ),
  valueEqual(
    ~title="findMap",
    ~expectation="nested options - find Some(None) (must return as Some(Some(None)))",
    ~a=() =>
      [Some(Some(1)), None, Some(None)]
      ->S.fromArray
      ->S.findMap(i =>
        switch i {
        | Some(None) => Some(Some(None))
        | _ => None
        }
      )
      ->Option.getExn,
    ~b=Some(None),
  ),
  valueEqual(
    ~title="findMap",
    ~expectation="nested options - find None",
    ~a=() =>
      [Some(Some(1)), None, Some(None)]
      ->S.fromArray
      ->S.findMap(i => i->Option.isNone ? Some(None) : None)
      ->Option.getExn,
    ~b=None,
  ),
  valueEqual(
    ~title="findMapi",
    ~expectation="",
    ~a=() => S.range(1, 5)->S.findMapi((n, inx) => n == 3 && inx == 2 ? Some("x") : None),
    ~b=Some("x"),
  ),
])

let toArrayTests = [
  valueEqual(
    ~title="toArray",
    ~expectation="when empty sequence, no generator functions are called",
    ~a=() => {
      let calls = ref(0)
      S.unfold(0, i => {
        calls := calls.contents + 1
        i < 100 ? Some(i, i + 1) : None
      })
      ->S.take(0)
      ->S.toArray
      ->ignore
      calls.contents
    },
    ~b=0,
  ),
]

let lengthTests = [
  valueEqual(~title="length", ~expectation="if empty => 0", ~a=() => S.empty->S.length, ~b=0),
  valueEqual(~title="length", ~expectation="if not empty", ~a=() => S.range(1, 5)->S.length, ~b=5),
  valueEqual(
    ~title="length",
    ~expectation="millions",
    ~a=() => S.replicate(999_999, 1)->S.length,
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
  valueEqual(
    ~title="equals",
    ~expectation=`${xs},${ys} => ${expected ? "true" : "false"}`,
    ~a=() => {
      let xs = xs->characters
      let ys = ys->characters
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
  valueEqual(
    ~title="compare",
    ~expectation=`${xs},${ys} => ${expected->intToString}`,
    ~a=() => {
      let xs = xs->characters
      let ys = ys->characters
      S.compare(xs, ys, Ex.Cmp.string)
    },
    ~b=expected,
  )
)

let headTailTests = [
  valueEqual(
    ~title="headTail",
    ~expectation="when empty => None",
    ~a=() => S.empty->S.headTail,
    ~b=None,
  ),
  valueEqual(
    ~title="headTail",
    ~expectation="when once => Some(head,empty)",
    ~a=() => S.once(4)->S.headTail->Option.map(((h, t)) => (h, t->S.toArray)),
    ~b=Some(4, []),
  ),
  valueEqual(
    ~title="headTail",
    ~expectation="when many items => Some(head,tail)",
    ~a=() => [1, 2, 3]->S.fromArray->S.headTail->Option.map(((h, t)) => (h, t->S.toArray)),
    ~b=Some(1, [2, 3]),
  ),
]

let tailTests = [
  willNotThrow(~title="tail", ~expectation="lazy", () => death()->S.tail),
  seqEqual(~title="tail", ~expectation="if empty => empty", ~a=() => S.empty->S.tail, ~b=[]),
  seqEqual(~title="tail", ~expectation="if singleton => empty", ~a=() => S.once(1)->S.tail, ~b=[]),
  seqEqual(
    ~title="tail",
    ~expectation="if many => get tail",
    ~a=() => S.range(1, 5)->S.tail,
    ~b=[2, 3, 4, 5],
  ),
]

let headTests = [
  valueEqual(~title="head", ~expectation="when empty", ~a=() => S.empty->S.head, ~b=None),
  valueEqual(
    ~title="head",
    ~expectation="when not empty",
    ~a=() => S.range(1, 5)->S.head,
    ~b=Some(1),
  ),
]

let minByMaxByTests = [
  valueEqual(
    ~title="minBy",
    ~expectation="when no items => None",
    ~a=() => S.empty->S.minBy(Ex.Cmp.int),
    ~b=None,
  ),
  valueEqual(
    ~title="minBy",
    ~expectation="when items => Some",
    ~a=() => [6, 7, 8, 3, 1, 3, 5, 8]->S.fromArray->S.minBy(Ex.Cmp.int),
    ~b=Some(1),
  ),
  valueEqual(
    ~title="maxBy",
    ~expectation="when no items => None",
    ~a=() => S.empty->S.maxBy(Ex.Cmp.int),
    ~b=None,
  ),
  valueEqual(
    ~title="maxBy",
    ~expectation="when items => Some",
    ~a=() => [6, 7, 8, 3, 1, 3, 5, 7]->S.fromArray->S.maxBy(Ex.Cmp.int),
    ~b=Some(8),
  ),
]

let isEmptyTests = [
  valueEqual(
    ~title="isEmpty",
    ~expectation="when empty => true",
    ~a=() => S.empty->S.isEmpty,
    ~b=true,
  ),
  valueEqual(
    ~title="isEmpty",
    ~expectation="when not empty => false",
    ~a=() => S.once(2)->S.isEmpty,
    ~b=false,
  ),
]

let joinTests = makeValueEqualTests(
  ~title="join",
  [
    (() => S.empty->S.join(""), "", "empty source"),
    (() => S.empty->S.join(","), "", "empty source"),
    (() => S.once("x")->S.join(""), "x", "singleton source"),
    (() => S.once("x")->S.join(","), "x", "singleton source"),
    (() => ["a", "b"]->S.fromArray->S.join(""), "ab", "two items in source"),
    (() => ["a", "b"]->S.fromArray->S.join(","), "a,b", "two items in source"),
    (() => ["a", "b", "c", "d"]->S.fromArray->S.join(""), "abcd", "many items in source"),
    (() => ["a", "b", "c", "d"]->S.fromArray->S.join(","), "a,b,c,d", "many items in source"),
  ],
)

let exactlyOneTests = makeValueEqualTests(
  ~title="exactlyOne",
  [
    (() => S.empty->S.exactlyOne, None, ""),
    (() => S.once(1)->S.exactlyOne, Some(1), ""),
    (() => S.range(1, 2)->S.exactlyOne, None, ""),
  ],
)

let isSortedByTests = makeValueEqualTests(
  ~title="isSortedBy",
  [
    (() => S.empty->S.isSortedBy(Ex.Cmp.int), true, ""),
    (() => S.once(3)->S.isSortedBy(Ex.Cmp.int), true, ""),
    (() => [1, 4, 4, 6, 7, 8, 9, 10]->S.fromArray->S.isSortedBy(Ex.Cmp.int), true, ""),
    (() => [1, 4, 4, 6, 7, 8, 0, 10]->S.fromArray->S.isSortedBy(Ex.Cmp.int), false, ""),
  ],
)

let toOptionTests = [
  valueEqual(
    ~title="toOption",
    ~expectation="when empty => None",
    ~a=() => S.empty->S.toOption,
    ~b=None,
  ),
  valueEqual(
    ~title="toOption",
    ~expectation="when once => Some",
    ~a=() => S.once(1)->S.toOption->Option.getWithDefault(S.empty)->S.toArray,
    ~b=[1],
  ),
  valueEqual(
    ~title="toOption",
    ~expectation="when many items => Some",
    ~a=() => S.range(1, 3)->S.toOption->Option.getWithDefault(S.empty)->S.toArray,
    ~b=[1, 2, 3],
  ),
]

let foldTests = {
  let add = (total, x) => total + x
  let lastSeen = (_: option<'a>, x: 'a) => Some(x)
  let oneUpTo = n => S.range(1, n)
  [
    (() => S.empty->S.fold(-1, add), -1),
    (() => S.once(99)->S.fold(1, add), 100),
    (() => S.range(1, 3)->S.fold(1, add), 7),
    (() => oneUpTo(99)->S.fold(None, lastSeen)->Option.getWithDefault(-1), 99),
    (() => oneUpTo(9999)->S.fold(None, lastSeen)->Option.getWithDefault(-1), 9999),
    (() => oneUpTo(999_999)->S.fold(None, lastSeen)->Option.getWithDefault(-1), 999_999),
  ]->Js.Array2.mapi(((a, b), index) =>
    valueEqual(~title="fold", ~expectation=`index ${index->intToString}`, ~a, ~b)
  )
}

let lastTests = {
  [
    (S.empty, None),
    (1->S.once, Some(1)),
    (S.range(1, 9), Some(9)),
    (S.range(1, 99), Some(99)),
    (S.range(1, 999), Some(999)),
    (S.range(1, 999999), Some(999999)),
  ]->Js.Array2.mapi(((xs, result), index) =>
    valueEqual(
      ~title="last",
      ~expectation=`index ${index->intToString}`,
      ~a=() => xs->S.last,
      ~b=result,
    )
  )
}

let (everyOkTests, everySomeTests) = {
  let validationTests = [
    ("when empty, return empty", [], Ok([])),
    ("when one error, return it", [30], Error("30")),
    ("when one ok, return it", [2], Ok([4])),
    ("when all ok, return all", [1, 2, 3], Ok([2, 4, 6])),
    ("when all error, return first", [20, 30, 40], Error("20")),
    ("when mix, return first error", [1, 2, 14, 3, 4], Error("14")),
  ]

  let validate = n => n < 10 ? Ok(n * 2) : Error(n->intToString)

  let everyOkTests =
    validationTests->Belt.Array.map(((expectation, input, expected)) =>
      valueEqual(
        ~title="everyOk",
        ~expectation,
        ~a=() => input->S.fromArray->S.map(validate)->S.everyOk->Result.map(i => i->S.toArray),
        ~b=expected,
      )
    )

  let everySomeTests = {
    validationTests->Belt.Array.map(((expectation, input, expected)) =>
      valueEqual(
        ~title="everySome",
        ~expectation,
        ~a=() =>
          input
          ->S.fromArray
          ->S.map(validate)
          ->S.map(Ex.Result.toOption)
          ->S.everySome
          ->Option.map(i => i->S.toArray),
        ~b=expected->Ex.Result.toOption,
      )
    )
  }
  (everyOkTests, everySomeTests)
}

let memoizeTests = [
  T.fromPredicate(
    ~category="Seq",
    ~title="cache",
    ~expectation="calculations only done once",
    () => {
      let randoms = S.foreverWith(() => Js.Math.random())->S.take(4)->S.cache
      let nums1 = randoms->S.toArray
      let nums2 = randoms->S.toArray
      let nums3 = randoms->S.toArray
      nums1 == nums2 && nums2 == nums3
    },
  ),
  T.fromPredicate(
    ~category="Seq",
    ~title="cache",
    ~expectation="all lazy; can cache foreverWith",
    () => {
      let randoms = S.foreverWith(() => Js.Math.random())->S.cache->S.take(4)
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
  (() => 1->S.once, i => i == 1, Some(1), "when once and found"),
  (() => 1->S.once, _ => false, None, "when once and predicate false"),
  (() => [1, 2, 3]->S.fromArray, i => i == 1, Some(1), "when many and is first"),
  (() => [1, 2, 3]->S.fromArray, i => i == 2, Some(2), "when many and is middle"),
  (() => [1, 2, 3]->S.fromArray, i => i == 3, Some(3), "when many and is last"),
  (() => [1, 2, 3]->S.fromArray, _ => false, None, "when many and predicate false"),
  (() => S.range(1, 999_999), i => i == 999_999, Some(999_999), "when million"),
  (() => S.range(1, 999_999), _ => false, None, "when million"),
]->Js.Array2.mapi(((source, predicate, result, note), index) =>
  T.fromPredicate(
    ~category="Seq",
    ~title="find",
    ~expectation=`${index->intToString} ${note}`,
    () => {
      source()->S.find(predicate) == result
    },
  )
)

let map2Tests = {
  let test = (xs, ys, expected) => {
    T.fromPredicate(
      ~category="Seq",
      ~title="map2",
      ~expectation=`${xs},${ys} => ${expected}`,
      () => {
        let xs = xs->characters
        let ys = ys->characters
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
    T.fromPredicate(
      ~category="Seq",
      ~title="map3",
      ~expectation=`${xs},${ys},${zs} => ${expected}`,
      () => {
        let xs = xs->characters
        let ys = ys->characters
        let zs = zs->characters
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

let consumeTests = [
  T.fromPredicate(
    ~category="Seq",
    ~title="consume",
    ~expectation="enumerates sequence for side effects",
    () => {
      let lastSeen = ref(0)
      S.range(0, 999_999)->S.tap(i => lastSeen := i)->S.consume
      lastSeen.contents == 999_999
    },
  ),
]

let reverseTests =
  makeSeqEqualsTests(
    ~title="reverse",
    [
      (S.empty->S.reverse, [], ""),
      (S.once(1)->S.reverse, [1], ""),
      (S.range(1, 5)->S.reverse, [5, 4, 3, 2, 1], ""),
    ],
  )->Js.Array2.concat([
    willNotThrow(~title="reverse", ~expectation="lazy; not reversed until iterated", () =>
      death()->S.reverse
    ),
  ])

let orElseTests = makeSeqEqualsTests(
  ~title="orElse",
  [
    (S.empty->S.orElse(S.empty), [], ""),
    (S.empty->S.orElse(S.range(1, 3)), [1, 2, 3], ""),
    (S.once(1)->S.orElse(S.range(4, 6)), [1], ""),
    (S.range(1, 3)->S.orElse(S.range(4, 6)), [1, 2, 3], ""),
  ],
)

let sortByTests = makeSeqEqualsTests(
  ~title="sortBy",
  [
    (S.empty->S.sortBy(intCmp), [], ""),
    (S.once(1)->S.sortBy(intCmp), [1], ""),
    ([1, 5, 2, 9, 7, 3]->S.fromArray->S.sortBy(intCmp), [1, 2, 3, 5, 7, 9], ""),
  ],
)->Js.Array2.concat([
  willNotThrow(~title="sortBy", ~expectation="lazy", () => death()->S.sortBy(intCmp)),
  T.fromPredicate(~category="Seq", ~title="sortBy", ~expectation="stable", () => {
    let sortByFirst = (a, b) => {
      let (afst, _) = a
      let (bfst, _) = b
      intCmp(afst, bfst)
    }
    let data = [
      (2, "x"),
      (1, "x"),
      (2, "y"),
      (4, "x"),
      (3, "x"),
      (1, "y"),
      (3, "y"),
      (2, "z"),
      (4, "y"),
      (1, "z"),
      (3, "z"),
      (4, "z"),
    ]
    let sorted = data->S.fromArray->S.sortBy(sortByFirst)->S.map(((_, letter)) => letter)->S.toArray
    sorted == "xyzxyzxyzxyz"->Js.String2.split("")
  }),
])

let delayTests =
  makeSeqEqualsTests(
    ~title="delay",
    [
      (S.delay(() => S.empty), [], ""),
      (S.delay(() => S.once(1)), [1], ""),
      (S.delay(() => S.range(1, 5)), [1, 2, 3, 4, 5], ""),
      (S.delay(() => Js.Exn.raiseError("boom!"))->S.take(0), [], ""),
    ],
  )->Js.Array2.concat([
    willNotThrow(~title="delay", ~expectation="lazy", () => S.delay(throwIfInvoked)),
  ])

let (combinationTests, permutationTests) = {
  let sortLetters = w =>
    w->String.split("")->Belt.SortArray.stableSortBy(stringCmp)->Js.Array2.joinWith("")
  let sortOutput = combos =>
    combos
    ->S.map(combo => combo->S.fold("", (sum, i) => sum ++ i)->sortLetters)
    ->S.sortBy(stringCmp)
    ->S.toArray
    ->Js.Array2.joinWith(",")
  let sort = words =>
    words
    ->Js.String2.split(",")
    ->Js.Array2.map(sortLetters)
    ->Belt.SortArray.stableSortBy(stringCmp)
    ->Js.Array2.joinWith(",")
  let combos = (letters, k) => letters->String.split("")->S.fromArray->S.combinations(k)
  let comboString = (letters, k) => combos(letters, k)->S.map(((_, combo)) => combo)->sortOutput
  let permutes = (letters, k) => letters->String.split("")->S.fromArray->S.permutations(k)
  let permuteString = (letters, k) => permutes(letters, k)->S.map(((_, combo)) => combo)->sortOutput
  let comboSamples = makeValueEqualTests(
    ~title="combinations",
    [
      (() => ""->comboString(1), ""->sort, ""),
      (() => "a"->comboString(1), "a"->sort, ""),
      (() => "a"->comboString(2), "a"->sort, ""),
      (() => "ab"->comboString(1), "a,b"->sort, ""),
      (() => "ab"->comboString(2), "a,b,ab"->sort, ""),
      (() => "ab"->comboString(3), "a,b,ab"->sort, ""),
      (() => "abc"->comboString(1), "a,b,c"->sort, ""),
      (() => "abc"->comboString(2), "a,b,c,ab,ac,bc"->sort, ""),
      (() => "abc"->comboString(3), "a,b,c,ab,ac,bc,abc"->sort, ""),
      (() => "abc"->comboString(4), "a,b,c,ab,ac,bc,abc"->sort, ""),
      (
        () =>
          "abc"
          ->combos(4)
          ->S.filterMap(((size, combo)) => size == 1 ? Some(combo) : None)
          ->sortOutput,
        "a,c,b"->sort,
        "check size == 1 in result",
      ),
      (
        () =>
          "abc"
          ->combos(4)
          ->S.filterMap(((size, combo)) => size == 3 ? Some(combo) : None)
          ->sortOutput,
        "abc"->sort,
        "check size == 3 in result",
      ),
      (
        () =>
          callCountForever()
          ->S.map(i => i == 1 ? "a" : i == 2 ? "b" : i == 3 ? "c" : "x")
          ->S.take(3)
          ->S.combinations(3)
          ->S.map(((_size, combo)) => combo)
          ->sortOutput,
        "a,b,c,ab,ac,bc,abc"->sort,
        "values appear cached",
      ),
    ],
  )
  let permuteSamples = makeValueEqualTests(
    ~title="permutations",
    [
      (() => ""->permuteString(1), ""->sort, ""),
      (() => "a"->permuteString(1), "a"->sort, ""),
      (() => "a"->permuteString(2), "a"->sort, ""),
      (() => "ab"->permuteString(1), "a,b"->sort, ""),
      (() => "ab"->permuteString(2), "a,b,ab,ba"->sort, ""),
      (() => "ab"->permuteString(3), "a,b,ab,ba"->sort, ""),
      (() => "abc"->permuteString(1), "a,b,c"->sort, ""),
      (() => "abc"->permuteString(2), "a,b,c,ab,ac,bc,ba,ca,cb"->sort, ""),
      (() => "abc"->permuteString(3), "a,b,c,ab,ac,bc,ba,ca,cb,abc,acb,bac,bca,cab,cba"->sort, ""),
      (() => "abc"->permuteString(4), "a,b,c,ab,ac,bc,ba,ca,cb,abc,acb,bac,bca,cab,cba"->sort, ""),
      (
        () =>
          "abc"
          ->permutes(4)
          ->S.filterMap(((size, combo)) => size == 1 ? Some(combo) : None)
          ->sortOutput,
        "a,c,b"->sort,
        "check size == 1 in result",
      ),
      (
        () =>
          "abc"
          ->permutes(4)
          ->S.filterMap(((size, combo)) => size == 3 ? Some(combo) : None)
          ->sortOutput,
        "abc,acb,bac,bca,cab,cba"->sort,
        "check size == 3 in result",
      ),
      (
        () =>
          callCountForever()
          ->S.map(i => i == 1 ? "a" : i == 2 ? "b" : i == 3 ? "c" : "x")
          ->S.take(3)
          ->S.permutations(3)
          ->S.map(((_size, combo)) => combo)
          ->sortOutput,
        "a,b,c,ab,ac,bc,ba,ca,cb,abc,acb,bac,bca,cab,cba"->sort,
        "values appear cached",
      ),
    ],
  )
  let miscellaneous = (title, f) => [
    willThrow(~title, ~expectation="if size < 0 throw", () => S.range(1, 9)->f(-1)),
    willThrow(~title, ~expectation="if size = 0 throw", () => S.range(1, 9)->f(0)),
    valueEqual(
      ~title,
      ~expectation="millions - take 10",
      ~a=() => S.range(1, 1000)->f(1000)->S.take(10)->S.last->Belt.Option.isSome,
      ~b=true,
    ),
    valueEqual(
      ~title,
      ~expectation="infinite - take 0",
      ~a=() => S.foreverWith(() => 1)->f(1000)->S.take(0)->S.last,
      ~b=None,
    ),
    valueEqual(
      ~title,
      ~expectation="infinite - take 1",
      ~a=() => S.foreverWith(() => 1)->f(1000)->S.take(4)->S.last->Option.isSome,
      ~b=true,
    ),
    valueEqual(
      ~title,
      ~a=() => S.replicateWith(3, throwIfInvoked)->f(1000)->S.take(0)->S.last,
      ~b=None,
      ~expectation="totally lazy",
    ),
  ]
  let combinationTests =
    [comboSamples, miscellaneous("combinations", S.combinations)]->Belt.Array.flatMap(i => i)
  let permutationTests =
    [permuteSamples, miscellaneous("permutations", S.permutations)]->Belt.Array.flatMap(i => i)
  (combinationTests, permutationTests)
}

let toListTests = makeValueEqualTests(
  ~title="toList",
  [
    (() => S.empty->S.toList, list{}, ""),
    (() => S.once(1)->S.toList, list{1}, ""),
    (() => [1, 2]->S.fromArray->S.toList, list{1, 2}, ""),
    (() => [1, 2, 3]->S.fromArray->S.toList, list{1, 2, 3}, ""),
    (() => S.range(1, 9_999)->S.toList->Belt.List.drop(9_998)->Option.getExn, list{9_999}, ""),
  ],
)

let sampleFibonacci = {
  let fib = Extras__SeqSamples.fibonacci
  makeSeqEqualsTests(
    ~title="sampleFibonacci",
    [(fib(12)->S.fromArray, [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89], "")],
  )
}

let sampleZipLongest = {
  let z = Extras__SeqSamples.zipLongest
  let compute = (xx, yy) => z(xx->S.fromArray, yy->S.fromArray)
  makeSeqEqualsTests(
    ~title="sampleZipLongest",
    [
      (
        compute([1, 2], ["x", "y", "z"]),
        [(Some(1), Some("x")), (Some(2), Some("y")), (None, Some("z"))],
        "",
      ),
      (compute([], ["x", "y", "z"]), [(None, Some("x")), (None, Some("y")), (None, Some("z"))], ""),
    ],
  )
}

let sampleBinaryDigits = {
  let getDigits = Extras__SeqSamples.binary
  makeSeqEqualsTests(
    ~title="sampleBinaryDigits",
    [
      (15->getDigits, [1, 1, 1, 1], ""),
      (8->getDigits, [1, 0, 0, 0], ""),
      (435_195->getDigits, [1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1], ""),
    ],
  )
}

let sampleChunkBySize = {
  let getChunks = Extras__SeqSamples.chunk
  makeSeqEqualsTests(
    ~title="sampleChunkBySize",
    [
      ([1, 2, 3]->getChunks(2), [[1, 2], [3]], ""),
      ([]->getChunks(8), [], ""),
      ([1, 2, 3]->getChunks(1), [[1], [2], [3]], ""),
    ],
  )
}

let sampleLocalMinimums = {
  let getMins = Extras__SeqSamples.localMinimums
  [
    valueEqual(
      ~title="sampleLocalMinimums",
      ~a=() => [(4, 4), (7, 0), (10, 12)]->getMins,
      ~b="(7, 0)",
      ~expectation="",
    ),
    valueEqual(
      ~title="sampleLocalMinimums",
      ~a=() => [(4, 4), (7, 0), (10, 12), (15, -1), (99, 3)]->getMins,
      ~b="(7, 0), (15, -1)",
      ~expectation="",
    ),
    valueEqual(
      ~title="sampleLocalMinimums",
      ~a=() => [(4, 4), (7, 19), (10, 99)]->getMins,
      ~b="There are no local minimums.",
      ~expectation="",
    ),
  ]
}

let sampleRunningTotal = {
  let f = Extras__SeqSamples.runningTotal
  makeSeqEqualsTests(
    ~title="sampleRunningTotal",
    [
      ([1, 2, 3, 4]->f, [0, 1, 3, 6, 10], ""),
      ([]->f, [0], ""),
      ([3, 3, 3, 3]->f, [0, 3, 6, 9, 12], ""),
    ],
  )
}

let tests = [
  // chunkByKindTests,
    allPairsTests,
    chunkBySizeTests,
    combinationTests,
    compareTests,
    concatTests,
    consTests,
    consumeTests,
    cycleTests,
    delayTests,
    dropTests,
    dropUntilTests,
    dropWhileTests,
    emptyTests,
    equalsTests,
    everyOkTests,
    everySomeTests,
    everyTests,
    exactlyOneTests,
    filterMapiTests,
    filterMapTests,
    filterOkTests,
    filterSomeTests,
    filterTests,
    findMapTests,
    findTests,
    flatMapTests,
    flattenTests,
  foldTests,
    forEachTests,
    foreverTests,
    foreverWithTests,
    fromArrayTests,
    fromListTests,
    fromOptionTests,
    headTailTests,
    headTests,
    indexedTests,
    initTests,
    interleaveTests,
    intersperseTests,
    intersperseWithTests,
    isEmptyTests,
    isSortedByTests,
    iterateTests,
    joinTests,
    lastTests,
    lengthTests,
    map2Tests,
    map3Tests,
    mapTests,
    memoizeTests,
    minByMaxByTests,
    onceTests,
    onceWithTests,
    orElseTests,
    pairwiseTests,
    permutationTests,
    prependTests,
    rangeMapTests,
    rangeTests,
    replicateTests,
    replicateWithTests,
    reverseTests,
    sampleBinaryDigits,
    sampleChunkBySize,
    sampleFibonacci,
    sampleLocalMinimums,
    sampleRunningTotal,
    sampleZipLongest,
    scanTests,
    someTests,
    sortByTests,
    sortedMergeTests,
  tailTests,
    takeTests,
    takeUntilTests,
    takeWhileTests,
    tapTests,
    toArrayTests,
    toListTests,
    toOptionTests,
    unfoldTests,
    windowAheadTests,
    windowBehindTests,
    windowTests,
  ]->Belt.Array.flatMap(i => i)
