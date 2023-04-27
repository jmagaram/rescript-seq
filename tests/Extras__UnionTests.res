module Ex = Extras
module Union = Extras__Union
module Literal = Extras__Literal
module Unknown = Extras__Unknown
module ResultEx = Extras__Result
module OptionEx = Extras__Option
module Test = Extras__Test
module ArrayEx = Extras__Array
module Pattern = Extras__Pattern
module Result = Belt.Result
module Option = Belt.Option

// ================================================
// Simple union of any string and the literal false
// ================================================

module StringOrFalseTests = {
  // This is the simplest way to generate a discriminated union with this
  // library. However the different cases are named `A` and `B` and matching
  // uses a function like `let match = (value, ~onA, ~onB)`, so it is not as
  // intuitive to use as it can be.
  module StringOrFalseBasic = Union.Make2({
    module A = Pattern.String
    module B = Literal.False
  })

  // Here is one way to extend the generated module with friendly function
  // names.
  module StringOrFalse = {
    include Union.Make2({
      module A = Pattern.String
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
    let test = (~expectation, predicate) =>
      Test.fromPredicate(~category="Union", ~title="StringOrFalse", ~expectation, predicate)
    [
      test(~expectation="fromString (safe assignment)", () =>
        "abc"->SF.fromString->SF.equals("abc"->Obj.magic)
      ),
      test(~expectation="fromFalse (safe assignment)", () =>
        Literal.False.value->SF.fromFalse->SF.equals(false->Obj.magic)
      ),
      test(~expectation="make from false => Some", () =>
        false->SF.make->OptionEx.isSomeAnd(v => v->Obj.magic == false)
      ),
      test(~expectation="make from true => None", () => true->SF.make->Option.isNone),
      test(~expectation="make from string => Some", () =>
        "abc"->SF.make->OptionEx.isSomeAnd(v => v->Obj.magic == "abc")
      ),
      test(~expectation="make from int => None", () => 34->SF.make->Option.isNone),
      test(~expectation="match on string", () =>
        "abc"->SF.fromString->SF.match(~onString=i => i == "abc", ~onFalse=_ => false)
      ),
      test(~expectation="equals when both false => true", () =>
        SF.equals(Literal.False.value->SF.fromFalse, Literal.False.value->SF.fromFalse)
      ),
      test(~expectation="equals when both string and same string => true", () =>
        SF.equals("abc"->SF.fromString, "abc"->SF.fromString)
      ),
      test(~expectation="equals when both string but different => false", () =>
        false == SF.equals("abc"->SF.fromString, "xyz"->SF.fromString)
      ),
      test(~expectation="equals when one string and one false => false", () =>
        false == SF.equals("abc"->SF.fromString, Literal.False.value->SF.fromFalse)
      ),
      test(~expectation="equals when one string and one false => false", () =>
        false == SF.equals("abc"->SF.fromString, Literal.False.value->SF.fromFalse)
      ),
      test(~expectation="toString when string => Some", () =>
        "abc"->SF.fromString->SF.toString == Some("abc")
      ),
      test(~expectation="toString when not string => None", () =>
        Literal.False.value->SF.fromFalse->SF.toString->Option.isNone
      ),
      test(~expectation="toFalse when false => Some", () =>
        Literal.False.value->SF.fromFalse->SF.toFalse == Some(Literal.False.value)
      ),
      test(~expectation="toFalse when not false => None", () =>
        "abc"->SF.fromString->SF.toFalse->Option.isNone
      ),
    ]
  }
}

// =====================
// Other simple examples
// =====================

// option<string> | int
module StringOptionOrInt = Union.Make2({
  module A = Pattern.MakeOption(Pattern.String)
  module B = Pattern.Int
})

// (string, bool) | int
module TupleOrString = Union.Make2({
  module A = Pattern.MakeTuple2({
    module A = Pattern.String
    module B = Pattern.Bool
  })
  module B = Pattern.Int
})

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
    let test = (~expectation, predicate) =>
      Test.fromPredicate(~category="Union", ~title="Fancy", ~expectation, predicate)
    [
      test(~expectation="fromNegativeOne", () =>
        NegativeOne.value->Target.fromNegativeOne->Obj.magic == -1
      ),
      test(~expectation="fromNull", () =>
        Literal.Null.value->Target.fromNull->Obj.magic == Js.null
      ),
      test(~expectation="fromSuccess", () => {
        let value = {"success": Literal.True.value, "count": 5}
        value->Target.fromSuccess->Obj.magic == value
      }),
      test(~expectation="make from NegativeOne => Some", () =>
        NegativeOne.value->Target.make->Obj.magic == Some(-1)
      ),
      test(~expectation="make from invalid value like 34 => None", () =>
        34->Target.make->Option.isNone
      ),
      test(~expectation="match on Success", () =>
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
      test(~expectation="match on NegativeOne", () =>
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
      test(~expectation="toNull when null => Some", () =>
        Literal.Null.value->Target.fromNull->Target.toNull->Option.isSome
      ),
      test(~expectation="toNull when something else => None", () =>
        NegativeOne.value->Target.fromNegativeOne->Target.toNull->Option.isNone
      ),
      test(~expectation="match on Null", () =>
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
  module Point2D = {
    type t = (float, float)
    let isTypeOf = u =>
      // lightweight validation; just check length
      Js.Array2.isArray(u) && Js.Array2.length((Obj.magic(u): array<unknown>)) == 2
    let equals = (x: t, y: t) => x == y
  }

  module Point3D = {
    type t = (float, float, float)
    let isTypeOf = u =>
      // lightweight validation; just check length
      Js.Array2.isArray(u) && Js.Array2.length((Obj.magic(u): array<unknown>)) == 3
    let equals = (x: t, y: t) => x == y
  }

  module Point = Union.Make2({
    module A = Point2D
    module B = Point3D
  })

  let test = (~expectation, predicate) =>
    Test.fromPredicate(~category="Union", ~title="Point", ~expectation, predicate)

  let tests = [
    test(~expectation="make from 2d => Some", () => [1.0, 1.0]->Point.make->Option.isSome),
    test(~expectation="make from 3d => Some", () => [1.0, 1.0, 1.0]->Point.make->Option.isSome),
    test(~expectation="make from 4d => None", () =>
      [1.0, 1.0, 1.0, 1.0]->Point.make->Option.isNone
    ),
    test(~expectation="make from empty => None", () => []->Point.make->Option.isNone),
    test(~expectation="make from string => None", () => "abc"->Point.make->Option.isNone),
    test(~expectation="match on 2d", () => {
      let p = (1.0, 1.0)
      p->Point.fromA->Point.matchAB(~onA=i => i === p, ~onB=_ => false)
    }),
    test(~expectation="match on 3d", () => {
      let p = (1.0, 1.0, 1.0)
      p->Point.fromB->Point.matchAB(~onA=_ => false, ~onB=i => i === p)
    }),
    test(~expectation="match on 3d", () => {
      (2.0, 3.0, 4.0)
      ->Point.fromB
      ->Point.matchAB(~onA=_ => false, ~onB=((x, y, z)) => x === 2.0 && y === 3.0 && z === 4.0)
    }),
  ]
}

// =============================================================================
// Use type-safe JSON parsing to determine the shape and validity of each choice
// in the union. ONLY USING RescriptStruct here to see how it compares to using
// the Union module in this library. Attempting to build a union with easy-to-use
// constructors, equality, and pattern matching.
//
// A: | string // between 5 and 10 chacters long
// B: | int    // non-negative
// C: | { "x": int, "y": int }
// =============================================================================

module OnlyRescriptStructTests = {
  open RescriptStruct

  module ShortString = {
    @unboxed type t = Short(string)
    let struct =
      S.string()->S.String.min(3)->S.String.max(10)->S.transform(~parser=v => Short(v), ())
    let make = s => s->S.parseAnyWith(struct)->ResultEx.toOption
    let equals = (x: t, y: t) => x === y
  }

  module NonNegativeInt = {
    @unboxed type t = NonNegative(int)
    let struct = S.int()->S.Int.min(0)->S.transform(~parser=v => NonNegative(v), ())
    let make = n => n->S.parseAnyWith(struct)->ResultEx.toOption
    let equals = (x: t, y: t) => x === y
  }

  module Point = {
    type t = {x: int, y: int}
    let struct = S.object(o => {
      x: o->S.field("x", S.int()),
      y: o->S.field("y", S.int()),
    })
    let make = (x, y) => {x, y}
    let equals = (a: t, b: t) => a.x === b.x && a.y === b.y
  }

  module type TargetType = {
    type t

    external fromNonNegativeInt: NonNegativeInt.t => t = "%identity"
    external fromShortString: ShortString.t => t = "%identity"
    external fromPoint: Point.t => t = "%identity"

    let toPoint: t => option<Point.t>
    let toNonNegativeInt: t => option<NonNegativeInt.t>
    let toShortString: t => option<ShortString.t>

    let make: 'a => option<t>

    let match: (
      t,
      ~onPoint: Point.t => 'a,
      ~onInt: NonNegativeInt.t => 'a,
      ~onString: ShortString.t => 'a,
    ) => 'a

    let equals: (t, t) => bool
  }

  module Target: TargetType = {
    type t

    let makeUnsafe = (i): t => Obj.magic(i)

    let unionStruct: S.t<t> = S.union([
      Point.struct->S.transform(~parser=makeUnsafe, ()),
      ShortString.struct->S.transform(~parser=makeUnsafe, ()),
      NonNegativeInt.struct->S.transform(~parser=makeUnsafe, ()),
    ])

    external fromNonNegativeInt: NonNegativeInt.t => t = "%identity"
    external fromShortString: ShortString.t => t = "%identity"
    external fromPoint: Point.t => t = "%identity"

    let toPoint = (i: t) => i->S.parseAnyWith(Point.struct)->ResultEx.toOption
    let toShortString = (i: t) => i->S.parseAnyWith(ShortString.struct)->ResultEx.toOption
    let toNonNegativeInt = (i: t) => i->S.parseAnyWith(NonNegativeInt.struct)->ResultEx.toOption

    let make = i => i->S.parseAnyWith(unionStruct)->ResultEx.toOption

    let match = (value, ~onPoint, ~onInt, ~onString) => {
      let result =
        toPoint(value)
        ->Option.map(onPoint)
        ->OptionEx.orElseWith(() => toNonNegativeInt(value)->Option.map(onInt))
        ->OptionEx.orElseWith(() => toShortString(value)->Option.map(onString))
      switch result {
      | Some(v) => v
      | None => Js.Exn.raiseError("Unsafely cast value; did not pattern match.")
      }
    }

    let equalsBy = (f, eq, x, y) =>
      f(x)->Option.map(x => f(y)->Option.mapWithDefault(false, y => eq(x, y)))

    let equals = (x, y) => {
      equalsBy(toPoint, Point.equals, x, y)
      ->OptionEx.orElseWith(() => equalsBy(toNonNegativeInt, NonNegativeInt.equals, x, y))
      ->OptionEx.orElseWith(() => equalsBy(toShortString, ShortString.equals, x, y))
      ->Option.getWithDefault(false)
    }
  }

  let test = (~expectation, predicate) =>
    Test.fromPredicate(
      ~category="Union",
      ~title="Use RescriptStruct exclusively",
      ~expectation,
      predicate,
    )

  let tests = [
    test(~expectation="make from short string => Some", () =>
      "abc"->Target.make->OptionEx.isSomeAnd(v => Obj.magic(v) == "abc")
    ),
    test(~expectation="make from long string => None", () =>
      "abcdefghijklmnopqrstuv"->Target.make->Option.isNone
    ),
    test(~expectation="make from positive int => Some", () =>
      34
      ->NonNegativeInt.make
      ->Option.getExn
      ->Target.make
      ->OptionEx.isSomeAnd(i => Obj.magic(i) == 34)
    ),
    test(~expectation="make from negative number => None", () => -99->Target.make->Option.isNone),
    test(~expectation="make from point => Some", () =>
      Point.make(2, 3)
      ->Target.make
      ->OptionEx.isSomeAnd(v => Point.equals(Obj.magic(v), Point.make(2, 3)))
    ),
    test(~expectation="make from point => Some with exact same instance", () => {
      let p = Point.make(2, 3)
      Obj.magic(p->Target.fromPoint) === p
    }),
    test(~expectation="match on point", () => {
      Point.make(2, 3)
      ->Target.make
      ->Option.getExn
      ->Target.match(~onString=_ => false, ~onInt=_ => false, ~onPoint=p => p.x == 2 && p.y == 3)
    }),
    test(~expectation="match on positive int", () => {
      16
      ->Target.make
      ->Option.getExn
      ->Target.match(~onString=_ => false, ~onInt=i => i == NonNegative(16), ~onPoint=_ => false)
    }),
    test(~expectation="equality of int", () =>
      Target.equals(
        NonNegativeInt.make(1)->Option.getExn->Target.fromNonNegativeInt,
        NonNegativeInt.make(1)->Option.getExn->Target.fromNonNegativeInt,
      )
    ),
    test(~expectation="equality of int", () =>
      false ==
        Target.equals(
          NonNegativeInt.make(1)->Option.getExn->Target.fromNonNegativeInt,
          NonNegativeInt.make(2)->Option.getExn->Target.fromNonNegativeInt,
        )
    ),
    test(~expectation="equality of int and string", () =>
      false ==
        Target.equals(
          NonNegativeInt.make(1)->Option.getExn->Target.fromNonNegativeInt,
          ShortString.make("abcde")->Option.getExn->Target.fromShortString,
        )
    ),
    test(~expectation="equality of point", () =>
      Target.equals(Point.make(1, 2)->Target.fromPoint, Point.make(1, 2)->Target.fromPoint)
    ),
    test(~expectation="equality of point", () =>
      false == Target.equals(Point.make(1, 2)->Target.fromPoint, Point.make(9, 9)->Target.fromPoint)
    ),
  ]
}

// =============================================================================
// Use type-safe JSON parsing to determine the shape and validity of each choice
// in the union. But putting each option together using the Union module of this
// library.
//
// A: | string // between 5 and 10 chacters long
// B: | int    // non-negative
// C: | { "x": int, "y": int }
// =============================================================================
// 92 lines : Using RescriptStruct exclusively with explicit module type
// 70 lines : Using RescriptStruct exclusively without module type
// 44 lines : Using Union module defined in this package
// =============================================================================

module WithHelpFromRescriptStruct = {
  open RescriptStruct

  module ShortString = {
    @unboxed type t = Short(string)
    let struct =
      S.string()->S.String.min(3)->S.String.max(10)->S.transform(~parser=v => Short(v), ())
    let make = (s: string) => s->S.parseAnyWith(struct)->ResultEx.toOption
    let isTypeOf = (s: unknown) => s->S.parseAnyWith(struct)->Result.isOk
    let equals = (x: t, y: t) => x === y
  }

  module NonNegativeInt = {
    @unboxed type t = NonNegative(int)
    let struct = S.int()->S.Int.min(0)->S.transform(~parser=v => NonNegative(v), ())
    let make = (n: int) => n->S.parseAnyWith(struct)->ResultEx.toOption
    let isTypeOf = (s: unknown) => s->S.parseAnyWith(struct)->Result.isOk
    let equals = (x: t, y: t) => x === y
  }

  module Point = {
    type t = {x: int, y: int}
    let struct = S.object(o => {
      x: o->S.field("x", S.int()),
      y: o->S.field("y", S.int()),
    })
    let make = (x, y) => {x, y}
    let isTypeOf = (s: unknown) => s->S.parseAnyWith(struct)->Result.isOk
    let equals = (a: t, b: t) => a.x === b.x && a.y === b.y
  }

  module Target = {
    include Union.Make3({
      module A = ShortString
      module B = NonNegativeInt
      module C = Point
    })
    let fromString = fromA
    let fromNonNegativeInt = fromB
    let fromPoint = fromC
    let toString = toA
    let toNonNegativeInt = toB
    let toPoint = toC
    let match = (~onString, ~onInt, ~onPoint) => matchABC(~onA=onString, ~onB=onInt, ~onC=onPoint)
  }
}

// ==================================================================
// Laziness tests; making sure parsing is done on an as-needed basis.
// ==================================================================

module LazinessTests = {
  module StringThrows: Extras__Pattern.T = {
    type t = string
    let isTypeOf = (_: unknown) => {
      Js.Exn.raiseError("Tried to parse a string.")->ignore
      true
    }
    let equals = (a: string, b: string) => a == b
  }

  module IntOrBoolOrFloatOrString = Union.Make4({
    module A = Pattern.Int
    module B = Pattern.Bool
    module C = Pattern.Float
    module D = StringThrows
  })

  module IntOrBoolOrString = Union.Make3({
    module A = Pattern.Int
    module B = Pattern.Bool
    module C = StringThrows
  })

  module IntOrString = Union.Make2({
    module A = Pattern.Int
    module B = StringThrows
  })

  let test = (~expectation, predicate) =>
    Test.fromPredicate(~category="Union", ~title="Lazy evaluation", ~expectation, predicate)

  let tests = [
    test(~expectation="4 make from int => Some", () =>
      1->IntOrBoolOrFloatOrString.make->OptionEx.isSomeAnd(v => Obj.magic(v) == 1)
    ),
    test(~expectation="4 make from bool => Some", () =>
      true->IntOrBoolOrFloatOrString.make->OptionEx.isSomeAnd(v => Obj.magic(v) == true)
    ),
    test(~expectation="4 make from float => Some", () =>
      3.5->IntOrBoolOrFloatOrString.make->OptionEx.isSomeAnd(v => Obj.magic(v) == 3.5)
    ),
    test(~expectation="3 make from int => Some", () =>
      1->IntOrBoolOrString.make->OptionEx.isSomeAnd(v => Obj.magic(v) == 1)
    ),
    test(~expectation="3 make from bool => Some", () =>
      true->IntOrBoolOrString.make->OptionEx.isSomeAnd(v => Obj.magic(v) == true)
    ),
    test(~expectation="3 match from int", () =>
      1
      ->IntOrBoolOrString.fromA
      ->IntOrBoolOrString.matchABC(~onA=i => i == 1, ~onB=_ => false, ~onC=_ => false)
    ),
    test(~expectation="3 match from bool", () =>
      true
      ->IntOrBoolOrString.fromB
      ->IntOrBoolOrString.matchABC(~onA=_ => false, ~onB=b => b == true, ~onC=_ => false)
    ),
    test(~expectation="2 make from int => Some", () =>
      1->IntOrString.make->OptionEx.isSomeAnd(v => Obj.magic(v) == 1)
    ),
    test(~expectation="2 match from int", () =>
      1->IntOrString.fromA->IntOrString.matchAB(~onA=i => i == 1, ~onB=_ => false)
    ),
  ]
}

let tests =
  [
    StringOrFalseTests.tests,
    FancyUnionTest.tests,
    PointTests.tests,
    OnlyRescriptStructTests.tests,
    LazinessTests.tests,
  ]->Belt.Array.concatMany
