module T = Extras__Test
module L = Extras__Literal
module Unknown = Extras__Unknown
module Option = Belt.Option
module OptionEx = Extras__Option

module Seven = L.MakeInt({
  let value = 7
})

module Yes = L.MakeString({
  let value = "yes"
  let trimmed = true
  let caseInsensitive = true
})

let tests = [
  T.make(~category="Literal", ~title="True", ~expectation="all", ~predicate=() => {
    L.True.value->L.True.unwrap == true &&
    L.True.parse(35)->Option.isNone &&
    L.True.parse("abc")->Option.isNone &&
    L.True.parse(false)->Option.isNone &&
    L.True.parse(true)->OptionEx.isSomeAnd(i => L.True.equals(i, L.True.value)) &&
    L.True.equals(L.True.value, L.True.value) &&
    L.True.isTypeOf(true->Unknown.make) &&
    L.True.isTypeOf(false->Unknown.make) == false &&
    L.True.isTypeOf(34->Unknown.make) == false
  }),
  T.make(~category="Literal", ~title="Seven", ~expectation="all", ~predicate=() => {
    Seven.value->Seven.unwrap == 7 &&
    Seven.parse(35)->Option.isNone &&
    Seven.parse("abc")->Option.isNone &&
    Seven.parse(false)->Option.isNone &&
    Seven.parse(7)->OptionEx.isSomeAnd(i => Seven.equals(i, Seven.value)) &&
    Seven.equals(Seven.value, Seven.value) &&
    Seven.isTypeOf(7->Unknown.make) &&
    Seven.isTypeOf(34->Unknown.make) == false &&
    Seven.isTypeOf(false->Unknown.make) == false
  }),
  T.make(~category="Literal", ~title="Yes", ~expectation="all", ~predicate=() => {
    Yes.value->Yes.unwrap == "yes" &&
    Yes.parse(35)->Option.isNone &&
    Yes.parse("abc")->Option.isNone &&
    Yes.parse(false)->Option.isNone &&
    Yes.parse("  YES ")->OptionEx.isSomeAnd(i => Yes.equals(i, Yes.value)) &&
    Yes.parse("  yEs ")->OptionEx.isSomeAnd(i => Yes.equals(i, Yes.value)) &&
    Yes.equals(Yes.value, Yes.value) &&
    Yes.isTypeOf("yes"->Unknown.make) &&
    Yes.isTypeOf("   yEs"->Unknown.make) &&
    Yes.isTypeOf(34->Unknown.make) == false &&
    Yes.isTypeOf("no"->Unknown.make) == false
  }),
]
