module Ex = Extras
module Union = Extras__Union
module Literal = Extras__Literal
module Unknown = Extras__Unknown
module Option = Belt.Option
module OptionEx = Extras__Option
module Test = Extras__Test

// Ensure basic int, string, date, and user-defined literals can be pattern matched
module PatternTests = {
  let makeTest = (~title, ~guard, ~ok, ~invalid1, ~invalid2, ~invalid3) =>
    Test.make(~category="Patterns", ~title, ~expectation="", ~predicate=() =>
      true == ok->Js.Array2.every(i => i->Unknown.make->guard) &&
      false == invalid1->Unknown.make->guard &&
      false == invalid2->Unknown.make->guard &&
      false == invalid3->Unknown.make->guard
    )

  module Negative1 = Literal.MakeInt({
    let value = -1
  })

  module Yes = Literal.MakeString({
    let value = "yes"
    let trimmed = true
    let caseInsensitive = true
  })

  let tests = {
    [
      makeTest(
        ~title="True (bool literal)",
        ~guard=Literal.True.isTypeOf,
        ~ok=[true, Literal.True.value->Obj.magic],
        ~invalid1=false,
        ~invalid2=33,
        ~invalid3="abc",
      ),
      makeTest(
        ~title="Yes (string literal)",
        ~guard=Yes.isTypeOf,
        ~ok=["yes", "  YES", "  yEs"],
        ~invalid1="no",
        ~invalid2=33,
        ~invalid3=false,
      ),
      makeTest(
        ~title="Negative1 (int literal)",
        ~guard=Negative1.isTypeOf,
        ~ok=[-1],
        ~invalid1=0,
        ~invalid2=3.4,
        ~invalid3="abc",
      ),
      makeTest(
        ~title="Int",
        ~guard=Union.IntPattern.isTypeOf,
        ~ok=[1, -1, 34, Int32.max_int],
        ~invalid1="abc",
        ~invalid2=false,
        ~invalid3={"a": 1},
      ),
      makeTest(
        ~title="String",
        ~guard=Union.StringPattern.isTypeOf,
        ~ok=["abc", "", "   a b c"],
        ~invalid1=false,
        ~invalid2=43,
        ~invalid3=4.3,
      ),
      makeTest(
        ~title="Bool",
        ~guard=Union.BoolPattern.isTypeOf,
        ~ok=[true, false],
        ~invalid1=43,
        ~invalid2="abc",
        ~invalid3=4.3,
      ),
      makeTest(
        ~title="Date",
        ~guard=Union.DatePattern.isTypeOf,
        ~ok=[Js.Date.now()->Js.Date.fromFloat],
        ~invalid1="abc",
        ~invalid2=3,
        ~invalid3=Js.Date.fromString("abc"),
      ),
    ]
  }
}

// Really basic union of any string and the literal false

module StringOrFalseTests = {
  module StringOrFalse = {
    include Union.Make2({
      module A = Union.StringPattern
      module B = Literal.False
    })

    // Convenience functions so you don't have to remember that A is string and B is false
    let fromString = fromA
    let fromFalse = fromB
    let toString = toA
    let toFalse = toB
    let matchT = (value, ~onString, ~onFalse) => match(value, ~onA=onString, ~onB=onFalse)
  }

  module SF = StringOrFalse

  let tests = [
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="fromString (safe assignment)",
      ~predicate=() => "abc"->SF.fromString->SF.equals("abc"->Obj.magic),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="fromFalse (safe assignment)",
      ~predicate=() => Literal.False.value->SF.fromFalse->SF.equals(false->Obj.magic),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="make from false => Some",
      ~predicate=() => false->SF.make->OptionEx.isSomeAnd(v => v->Obj.magic == false),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="make from true => None",
      ~predicate=() => true->SF.make->Option.isNone,
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="make from string => Some",
      ~predicate=() => "abc"->SF.make->OptionEx.isSomeAnd(v => v->Obj.magic == "abc"),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="make from int => None",
      ~predicate=() => 34->SF.make->Option.isNone,
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="match on string",
      ~predicate=() =>
        "abc"->SF.fromString->SF.matchT(~onString=i => i == "abc", ~onFalse=_ => false),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="equals when both false => true",
      ~predicate=() =>
        SF.equals(Literal.False.value->SF.fromFalse, Literal.False.value->SF.fromFalse),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="equals when both string and same string => true",
      ~predicate=() => SF.equals("abc"->SF.fromString, "abc"->SF.fromString),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="equals when both string but different => false",
      ~predicate=() => false == SF.equals("abc"->SF.fromString, "xyz"->SF.fromString),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="equals when one string and one false => false",
      ~predicate=() => false == SF.equals("abc"->SF.fromString, Literal.False.value->SF.fromFalse),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="equals when one string and one false => false",
      ~predicate=() => false == SF.equals("abc"->SF.fromString, Literal.False.value->SF.fromFalse),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="toString when string => Some",
      ~predicate=() => "abc"->SF.fromString->SF.toString == Some("abc"),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="toString when not string => None",
      ~predicate=() => Literal.False.value->SF.fromFalse->SF.toString->Option.isNone,
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="toFalse when false => Some",
      ~predicate=() => Literal.False.value->SF.fromFalse->SF.toFalse == Some(Literal.False.value),
    ),
    Test.make(
      ~category="Union",
      ~title="StringOrFalse",
      ~expectation="toFalse when not false => None",
      ~predicate=() => "abc"->SF.fromString->SF.toFalse->Option.isNone,
    ),
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

let tests = [StringOrFalseTests.tests, exampleTests, PatternTests.tests]->Belt.Array.concatMany
