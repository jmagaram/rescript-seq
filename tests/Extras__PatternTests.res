module Test = Extras__Test
module Pattern = Extras__Pattern
module Unknown = Extras__Unknown
module Literal = Extras__Literal

// ================================================================================
// Ensure basic int, string, date, and user-defined literals can be pattern matched
// ================================================================================

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
      ~guard=Pattern.Int.isTypeOf,
      ~ok=[1, -1, 34, Int32.max_int],
      ~invalid1="abc",
      ~invalid2=false,
      ~invalid3={"a": 1},
    ),
    makeTest(
      ~title="String",
      ~guard=Pattern.String.isTypeOf,
      ~ok=["abc", "", "   a b c"],
      ~invalid1=false,
      ~invalid2=43,
      ~invalid3=4.3,
    ),
    makeTest(
      ~title="Bool",
      ~guard=Pattern.Bool.isTypeOf,
      ~ok=[true, false],
      ~invalid1=43,
      ~invalid2="abc",
      ~invalid3=4.3,
    ),
    makeTest(
      ~title="Date",
      ~guard=Pattern.Date.isTypeOf,
      ~ok=[Js.Date.now()->Js.Date.fromFloat],
      ~invalid1="abc",
      ~invalid2=3,
      ~invalid3=Js.Date.fromString("abc"),
    ),
  ]
}
