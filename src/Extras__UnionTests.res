module Ex = Extras
module Union = Extras__Union
module Literal = Extras__Literal
module Unknown = Extras__Unknown
module Option = Belt.Option
module OptionEx = Extras__Option
module Test = Extras__Test
module ArrayEx = Extras__Array

// ================================================
// Simple union of any string and the literal false
// ================================================

module StringOrFalseTests = {
  // This is the simplest way to generate a discriminated union with this
  // library. However the different cases are named `A` and `B` and matching
  // uses a function like `let match = (value, ~onA, ~onB)`, so it is not as
  // intuitive to use as it can be.
  module StringOrFalseBasic = Union.Make2({
    module A = Union.Patterns.String
    module B = Literal.False
  })

  // Here is one way to extend the generated module with friendly function
  // names.
  module StringOrFalse = {
    include Union.Make2({
      module A = Union.Patterns.String
      module B = Literal.False
    })
    let fromString = fromA // Assign a string to the type
    let fromFalse = fromB // Assign the literal false to the type
    let toString = toA // Convert the type to an option<string>
    let toFalse = toB // Convert the type to an option<false>
    let match = (value, ~onString, ~onFalse) => matchAB(value, ~onA=onString, ~onB=onFalse)
  }

  let tests = {
    module SF = StringOrFalse
    let test = (~expectation, ~predicate) =>
      Test.make(~category="Union", ~title="StringOrFalse", ~expectation, ~predicate)
    [
      test(~expectation="fromString (safe assignment)", ~predicate=() =>
        "abc"->SF.fromString->SF.equals("abc"->Obj.magic)
      ),
      test(~expectation="fromFalse (safe assignment)", ~predicate=() =>
        Literal.False.value->SF.fromFalse->SF.equals(false->Obj.magic)
      ),
      test(~expectation="make from false => Some", ~predicate=() =>
        false->SF.make->OptionEx.isSomeAnd(v => v->Obj.magic == false)
      ),
      test(~expectation="make from true => None", ~predicate=() => true->SF.make->Option.isNone),
      test(~expectation="make from string => Some", ~predicate=() =>
        "abc"->SF.make->OptionEx.isSomeAnd(v => v->Obj.magic == "abc")
      ),
      test(~expectation="make from int => None", ~predicate=() => 34->SF.make->Option.isNone),
      test(~expectation="match on string", ~predicate=() =>
        "abc"->SF.fromString->SF.match(~onString=i => i == "abc", ~onFalse=_ => false)
      ),
      test(~expectation="equals when both false => true", ~predicate=() =>
        SF.equals(Literal.False.value->SF.fromFalse, Literal.False.value->SF.fromFalse)
      ),
      test(~expectation="equals when both string and same string => true", ~predicate=() =>
        SF.equals("abc"->SF.fromString, "abc"->SF.fromString)
      ),
      test(~expectation="equals when both string but different => false", ~predicate=() =>
        false == SF.equals("abc"->SF.fromString, "xyz"->SF.fromString)
      ),
      test(~expectation="equals when one string and one false => false", ~predicate=() =>
        false == SF.equals("abc"->SF.fromString, Literal.False.value->SF.fromFalse)
      ),
      test(~expectation="equals when one string and one false => false", ~predicate=() =>
        false == SF.equals("abc"->SF.fromString, Literal.False.value->SF.fromFalse)
      ),
      test(~expectation="toString when string => Some", ~predicate=() =>
        "abc"->SF.fromString->SF.toString == Some("abc")
      ),
      test(~expectation="toString when not string => None", ~predicate=() =>
        Literal.False.value->SF.fromFalse->SF.toString->Option.isNone
      ),
      test(~expectation="toFalse when false => Some", ~predicate=() =>
        Literal.False.value->SF.fromFalse->SF.toFalse == Some(Literal.False.value)
      ),
      test(~expectation="toFalse when not false => None", ~predicate=() =>
        "abc"->SF.fromString->SF.toFalse->Option.isNone
      ),
    ]
  }
}

// ===========================================
// A: | { success: true, count: int}
// B: | { success: false, reason: string }
// C: | null
// D: | -1
// ===========================================
module FancyUnionTest = {
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

  // This is the union under test, with some convenience functions added.
  module Target = {
    include Union.Make4({
      module A = Success
      module B = Failure
      module C = Literal.Null
      module D = NegativeOne
    })

    // Convenience functions
    let fromSuccess = fromA
    let fromFailure = fromB
    let fromNull = fromC
    let fromNegativeOne = fromD
    let toSuccess = toA
    let toFailure = toB
    let toNull = toC
    let toNegativeOne = toD
    let match = (value, ~onSuccess, ~onFailure, ~onNull, ~onNegativeOne) =>
      matchABCD(value, ~onA=onSuccess, ~onB=onFailure, ~onC=onNull, ~onD=onNegativeOne)
  }

  let tests = {
    let test = (~expectation, ~predicate) =>
      Test.make(~category="Union", ~title="Fancy", ~expectation, ~predicate)
    [
      test(~expectation="fromNegativeOne", ~predicate=() =>
        NegativeOne.value->Target.fromNegativeOne->Obj.magic == -1
      ),
      test(~expectation="fromNull", ~predicate=() =>
        Literal.Null.value->Target.fromNull->Obj.magic == Js.null
      ),
      test(~expectation="fromSuccess", ~predicate=() => {
        let value = {"success": Literal.True.value, "count": 5}
        value->Target.fromSuccess->Obj.magic == value
      }),
      test(~expectation="make from NegativeOne => Some", ~predicate=() =>
        NegativeOne.value->Target.make->Obj.magic == Some(-1)
      ),
      test(~expectation="make from invalid value like 34 => None", ~predicate=() =>
        34->Target.make->Option.isNone
      ),
      test(~expectation="match on Success", ~predicate=() =>
        {"success": Literal.True.value, "count": 5}
        ->Target.make
        ->Option.getExn
        ->Target.match(
          ~onSuccess=i => i["count"] == 5,
          ~onFailure=_ => false,
          ~onNegativeOne=_ => false,
          ~onNull=_ => false,
        )
      ),
      test(~expectation="match on NegativeOne", ~predicate=() =>
        -1
        ->Target.make
        ->Option.getExn
        ->Target.match(
          ~onSuccess=_ => false,
          ~onFailure=_ => false,
          ~onNegativeOne=_ => true,
          ~onNull=_ => false,
        )
      ),
      test(~expectation="toNull when null => Some", ~predicate=() =>
        Literal.Null.value->Target.fromNull->Target.toNull->Option.isSome
      ),
      test(~expectation="toNull when something else => None", ~predicate=() =>
        NegativeOne.value->Target.fromNegativeOne->Target.toNull->Option.isNone
      ),
      test(~expectation="match on Null", ~predicate=() =>
        Literal.Null.value
        ->Target.fromNull
        ->Target.match(
          ~onSuccess=_ => false,
          ~onFailure=_ => false,
          ~onNegativeOne=_ => false,
          ~onNull=_ => true,
        )
      ),
    ]
  }
}

// ===========================================
// A: | (float, float) // 2-d
// B: | (float, float, float) // 3-d
// ===========================================
module PointTests = {
  module Point = Union.Make2({
    module A = {
      type t = (float, float)
      let isTypeOf = u =>
        // lightweight validation; just check length
        Js.Array2.isArray(u) && Js.Array2.length((Obj.magic(u): array<Unknown.t>)) == 2
      let equals = (x: t, y: t) => x == y
    }
    module B = {
      type t = (float, float, float)
      let isTypeOf = u =>
        // lightweight validation; just check length
        Js.Array2.isArray(u) && Js.Array2.length((Obj.magic(u): array<Unknown.t>)) == 3
      let equals = (x: t, y: t) => x == y
    }
  })

  let test = (~expectation, ~predicate) =>
    Test.make(~category="Union", ~title="Point", ~expectation, ~predicate)

  let tests = [
    test(~expectation="make from 2d => Some", ~predicate=() =>
      [1.0, 1.0]->Point.make->Option.isSome
    ),
    test(~expectation="make from 3d => Some", ~predicate=() =>
      [1.0, 1.0, 1.0]->Point.make->Option.isSome
    ),
    test(~expectation="make from 4d => None", ~predicate=() =>
      [1.0, 1.0, 1.0, 1.0]->Point.make->Option.isNone
    ),
    test(~expectation="make from empty => None", ~predicate=() => []->Point.make->Option.isNone),
    test(~expectation="make from string => None", ~predicate=() =>
      "abc"->Point.make->Option.isNone
    ),
    test(~expectation="match on 2d", ~predicate=() => {
      let p = (1.0, 1.0)
      p->Point.fromA->Point.matchAB(~onA=i => i === p, ~onB=_ => false)
    }),
    test(~expectation="match on 3d", ~predicate=() => {
      let p = (1.0, 1.0, 1.0)
      p->Point.fromB->Point.matchAB(~onA=_ => false, ~onB=i => i === p)
    }),
    test(~expectation="match on 3d", ~predicate=() => {
      (2.0, 3.0, 4.0)
      ->Point.fromB
      ->Point.matchAB(~onA=_ => false, ~onB=((x, y, z)) => x === 2.0 && y === 3.0 && z === 4.0)
    }),
  ]
}

// ================================================================================
// Ensure basic int, string, date, and user-defined literals can be pattern matched
// ================================================================================
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
        ~title="Null (null literal)",
        ~guard=Literal.Null.isTypeOf,
        ~ok=[Js.null->Obj.magic],
        ~invalid1=false,
        ~invalid2=33,
        ~invalid3="abc",
      ),
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
        ~guard=Union.Patterns.Int.isTypeOf,
        ~ok=[1, -1, 34, Int32.max_int],
        ~invalid1="abc",
        ~invalid2=false,
        ~invalid3={"a": 1},
      ),
      makeTest(
        ~title="String",
        ~guard=Union.Patterns.String.isTypeOf,
        ~ok=["abc", "", "   a b c"],
        ~invalid1=false,
        ~invalid2=43,
        ~invalid3=4.3,
      ),
      makeTest(
        ~title="Bool",
        ~guard=Union.Patterns.Bool.isTypeOf,
        ~ok=[true, false],
        ~invalid1=43,
        ~invalid2="abc",
        ~invalid3=4.3,
      ),
      makeTest(
        ~title="Date",
        ~guard=Union.Patterns.Date.isTypeOf,
        ~ok=[Js.Date.now()->Js.Date.fromFloat],
        ~invalid1="abc",
        ~invalid2=3,
        ~invalid3=Js.Date.fromString("abc"),
      ),
    ]
  }
}

let tests =
  [
    StringOrFalseTests.tests,
    FancyUnionTest.tests,
    PointTests.tests,
    PatternTests.tests,
  ]->Belt.Array.concatMany
