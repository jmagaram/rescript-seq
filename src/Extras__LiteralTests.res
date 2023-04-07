module T = Extras__Test
module L = Extras__Literal
module Unknown = Extras__Unknown
module Option = Belt.Option
module OptionEx = Extras__Option

let tests = [
  T.make(~category="Literal", ~title="True", ~expectation="", ~predicate=() => {
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
]

// when authoring "isTypeOf" want to use Unknown.t to be safe
// when using it, want it to be anything
