module Ex = Extras
module Union = Extras__Union
module Literal = Extras__Literal
module Unknown = Extras__Unknown
module Option = Belt.Option
module OptionEx = Extras__Option
module Test = Extras__Test

// Tests for basic Int, String, Date and other patterns

let patternTests = {
  let test = (~title, ~guard, ~value, ~expected) =>
    Test.make(~category="UnionPatterns", ~title, ~expectation="", ~predicate=() =>
      expected == value->Unknown.make->guard
    )
  [
    test(~title="Int", ~guard=Union.IntPattern.isTypeOf, ~value=43, ~expected=true),
    test(~title="Int", ~guard=Union.IntPattern.isTypeOf, ~value="abc", ~expected=false),
    test(~title="String", ~guard=Union.StringPattern.isTypeOf, ~value="abc", ~expected=true),
    test(~title="String", ~guard=Union.StringPattern.isTypeOf, ~value=false, ~expected=false),
    test(~title="Bool", ~guard=Union.BoolPattern.isTypeOf, ~value=true, ~expected=true),
    test(~title="Bool", ~guard=Union.BoolPattern.isTypeOf, ~value="abc", ~expected=false),
    test(
      ~title="Date",
      ~guard=Union.DatePattern.isTypeOf,
      ~value=Js.Date.now()->Js.Date.fromFloat,
      ~expected=true,
    ),
    test(
      ~title="Date",
      ~guard=Union.DatePattern.isTypeOf,
      ~value=Js.Date.fromString("abc"),
      ~expected=false,
    ),
    test(~title="Date", ~guard=Union.DatePattern.isTypeOf, ~value="abc", ~expected=false),
  ]
}
// Tests for a weird union like this:
//
// A: | { success: true, count: int}
// B: | { success: false, reason: string }
// C: | null
// D: | -1

module Success = {
  type t = {"success": Literal.True.t, "count": int}
  let isTypeOf = u => u->Unknown.getBool("success")->OptionEx.isSomeAnd(v => v == true)
  let equals = (x: t, y: t) => x["count"] == y["count"]
}

module Failure = {
  type t = {"success": Literal.False.t, "reason": string}
  let isTypeOf = u => u->Unknown.getBool("success")->OptionEx.isSomeAnd(v => v == false)
  let equals = (x: t, y: t) => x["reason"] == y["reason"]
}

module NegativeOne = Literal.MakeInt({
  let value = -1
})

module Example = Union.Make4({
  module A = Success
  module B = Failure
  module C = Literal.Null
  module D = NegativeOne
})

let exampleTests = [
  Test.make(
    ~category="Union",
    ~title="make",
    ~expectation="when is valid negative 1, return Some",
    ~predicate=() => -1->Example.make->Option.isSome,
  ),
  Test.make(
    ~category="Union",
    ~title="make",
    ~expectation="when is not valid 99, return None",
    ~predicate=() => 99->Example.make->Option.isNone,
  ),
  Test.make(
    ~category="Union",
    ~title="make",
    ~expectation="when is valid success, return Some",
    ~predicate=() => {"success": true, "count": 5}->Example.make->Option.isSome,
  ),
  Test.make(
    ~category="Union",
    ~title="make",
    ~expectation="when is not valid success, return None",
    ~predicate=() => {"success": "yes"}->Example.make->Option.isNone,
  ),
  Test.make(
    ~category="Union",
    ~title="pattern matching",
    ~expectation="can pattern match",
    ~predicate=() => {
      let v = -1->Example.make->Option.getExn
      v->Example.match(~onA=_ => false, ~onB=_ => false, ~onC=_ => false, ~onD=_ => true)
    },
  ),
  Test.make(
    ~category="Union",
    ~title="pattern matching",
    ~expectation="can pattern match",
    ~predicate=() => {
      let v = {"success": true, "count": 17}->Example.make->Option.getExn
      v->Example.match(
        ~onA=i => i["count"] == 17,
        ~onB=_ => false,
        ~onC=_ => false,
        ~onD=_ => false,
      )
    },
  ),
  Test.make(
    ~category="Union",
    ~title="equality",
    ~expectation="when same type and values return true",
    ~predicate=() => {
      let a = {"success": true, "count": 17}->Example.make->Option.getExn
      let b = {"success": true, "count": 17}->Example.make->Option.getExn
      Example.equals(a, b)
    },
  ),
  Test.make(
    ~category="Union",
    ~title="equality",
    ~expectation="when same type but different return false",
    ~predicate=() => {
      let a = {"success": true, "count": 17}->Example.make->Option.getExn
      let b = {"success": true, "count": 8}->Example.make->Option.getExn
      Example.equals(a, b) == false
    },
  ),
  Test.make(
    ~category="Union",
    ~title="equality",
    ~expectation="when different types return false",
    ~predicate=() => {
      let a = NegativeOne.value->Example.make->Option.getExn
      let b = {"success": false, "reason": "hmm..."}->Example.make->Option.getExn
      Example.equals(a, b) == false
    },
  ),
]

// Simple StringOrInt

module StringOrInt = Union.Make2({
  module A = Union.StringPattern
  module B = Union.IntPattern
})

// ArrayIndex

module ValidIndex: {
  include Union.Pattern with type t = int
  let make: int => option<t>
} = {
  type t = int
  let isTypeOf = u => u->Unknown.toFloat->OptionEx.isSomeAnd(i => i >= 0.0)
  let make = (n: int) => n >= 0 ? Some((Obj.magic(n): t)) : None
  let equals = (x, y) => x === y
}

module ArrayIndex = Union.Make2({
  module A = NegativeOne
  module B = ValidIndex
})

// Return all the automated tests

let tests = [exampleTests, patternTests]->Belt.Array.concatMany
