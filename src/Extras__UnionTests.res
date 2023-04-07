module Ex = Extras
module Union = Extras__Union
module Literal = Extras__Literal
module Unknown = Extras__Unknown
module Option = Belt.Option
module OptionEx = Extras__Option
module Test = Extras__Test

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

let tests = [
  Test.make(
    ~category="Union",
    ~title="make",
    ~expectation="when is valid, return Some",
    ~predicate=() => {
      -1->Example.make->Option.isSome
    },
  ),
  Test.make(
    ~category="Union",
    ~title="make",
    ~expectation="when is not valid, return None",
    ~predicate=() => {
      -2->Example.make->Option.isNone
    },
  ),
  Test.make(
    ~category="Union",
    ~title="make",
    ~expectation="when is valid, return Some",
    ~predicate=() => {
      {"success": true, "count": 5}->Example.make->Option.isSome
    },
  ),
  Test.make(
    ~category="Union",
    ~title="make",
    ~expectation="when is not valid, return None",
    ~predicate=() => {
      {"success": "yes"}->Example.make->Option.isNone
    },
  ),
  Test.make(
    ~category="Union",
    ~title="pattern matching",
    ~expectation="can pattern match",
    ~predicate=() => {
      let v = -1->Example.make->Option.getExn
      let result =
        v->Example.match(~onA=_ => false, ~onB=_ => false, ~onC=_ => false, ~onD=_ => true)
      result
    },
  ),
  Test.make(
    ~category="Union",
    ~title="pattern matching",
    ~expectation="can pattern match",
    ~predicate=() => {
      let v = {"success": true, "count": 17}->Example.make->Option.getExn
      let result =
        v->Example.match(
          ~onA=i => i["count"] == 17,
          ~onB=_ => false,
          ~onC=_ => false,
          ~onD=_ => false,
        )
      result
    },
  ),
  Test.make(
    ~category="Union",
    ~title="equality",
    ~expectation="when same return true",
    ~predicate=() => {
      let a = {"success": true, "count": 17}->Example.make->Option.getExn
      let b = {"success": true, "count": 17}->Example.make->Option.getExn
      Example.equals(a, b)
    },
  ),
  Test.make(
    ~category="Union",
    ~title="equality",
    ~expectation="when different return false",
    ~predicate=() => {
      let a = {"success": true, "count": 17}->Example.make->Option.getExn
      let b = {"success": true, "count": 8}->Example.make->Option.getExn
      Example.equals(a, b) == false
    },
  ),
  Test.make(
    ~category="Union",
    ~title="equality",
    ~expectation="when different return false",
    ~predicate=() => {
      let a = NegativeOne.value->Example.make->Option.getExn
      let b = {"success": false, "reason": "hmm..."}->Example.make->Option.getExn
      Example.equals(a, b) == false
    },
  ),
]
