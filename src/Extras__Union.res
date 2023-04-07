module Option = Belt.Option
module OptionEx = Extras__Option
module Unknown = Extras__Unknown

module type Pattern = {
  type t
  let isTypeOf: Unknown.t => bool
  let equals: (t, t) => bool
}

module PatternTools = (P: Pattern) => {
  let make = x => x->Unknown.make->P.isTypeOf ? Some((Obj.magic(x): P.t)) : None
  let eq = (x, y) => x->make->Option.flatMap(x => y->make->Option.map(y => P.equals(x, y)))
}

module Make4 = (
  P: {
    module A: Pattern
    module B: Pattern
    module C: Pattern
    module D: Pattern
  },
) => {
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t
  type d = P.D.t

  module A_Tools = PatternTools(P.A)
  module B_Tools = PatternTools(P.B)
  module C_Tools = PatternTools(P.C)
  module D_Tools = PatternTools(P.D)

  let fromA = (value: a): t => Obj.magic(value)
  let fromB = (value: b): t => Obj.magic(value)
  let fromC = (value: c): t => Obj.magic(value)
  let fromD = (value: d): t => Obj.magic(value)

  let make = value =>
    value
    ->A_Tools.make
    ->Option.map(fromA)
    ->Option.orElse(value->B_Tools.make->Option.map(fromB))
    ->Option.orElse(value->C_Tools.make->Option.map(fromC))
    ->Option.orElse(value->D_Tools.make->Option.map(fromD))

  let match = (value, ~onA, ~onB, ~onC, ~onD) =>
    switch value
    ->A_Tools.make
    ->Option.map(onA)
    ->Option.orElse(value->B_Tools.make->Option.map(onB))
    ->Option.orElse(value->C_Tools.make->Option.map(onC))
    ->Option.orElse(value->D_Tools.make->Option.map(onD)) {
    | Some(value) => value
    | None =>
      Js.Exn.raiseError("The value was unsafely cast and did not match any of the provided types.")
    }

  let equals = (x: t, y: t) =>
    A_Tools.eq(x, y)
    ->OptionEx.orElseWith(() => B_Tools.eq(x, y))
    ->OptionEx.orElseWith(() => C_Tools.eq(x, y))
    ->OptionEx.orElseWith(() => D_Tools.eq(x, y))
    ->Option.getWithDefault(false)
}

module Make3 = (
  P: {
    module A: Pattern
    module B: Pattern
    module C: Pattern
  },
) => {
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t

  let toA = value => P.A.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): a)) : None
  let toB = value => P.B.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): b)) : None
  let toC = value => P.C.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): c)) : None

  let fromA = (value: a): t => Obj.magic(value)
  let fromB = (value: b): t => Obj.magic(value)
  let fromC = (value: c): t => Obj.magic(value)

  let make = value =>
    value
    ->toA
    ->Option.map(fromA)
    ->Option.orElse(value->toB->Option.map(fromB))
    ->Option.orElse(value->toC->Option.map(fromC))

  let match = (value, ~onA, ~onB, ~onC) =>
    switch value
    ->toA
    ->Option.map(onA)
    ->Option.orElse(value->toB->Option.map(onB))
    ->Option.orElse(value->toC->Option.map(onC)) {
    | Some(value) => value
    | None =>
      Js.Exn.raiseError("The value was unsafely cast and did not match any of the provided types.")
    }

  let equals = (x: t, y: t) =>
    OptionEx.map2(toA(x), toA(y), P.A.equals)
    ->OptionEx.orElseWith(() => OptionEx.map2(toB(x), toB(y), P.B.equals))
    ->OptionEx.orElseWith(() => OptionEx.map2(toC(x), toC(y), P.C.equals))
    ->Option.getWithDefault(false)
}

module Make2 = (
  P: {
    module A: Pattern
    module B: Pattern
  },
) => {
  type t
  type a = P.A.t
  type b = P.B.t

  let toA = value => P.A.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): a)) : None
  let toB = value => P.B.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): b)) : None

  let fromA = (value: a): t => Obj.magic(value)
  let fromB = (value: b): t => Obj.magic(value)

  let make = value => value->toA->Option.map(fromA)->Option.orElse(value->toB->Option.map(fromB))

  let match = (value, ~onA, ~onB) =>
    switch value->toA->Option.map(onA)->Option.orElse(value->toB->Option.map(onB)) {
    | Some(value) => value
    | None =>
      Js.Exn.raiseError("The value was unsafely cast and did not match any of the provided types.")
    }

  let equals = (x: t, y: t) =>
    OptionEx.map2(toA(x), toA(y), P.A.equals)
    ->OptionEx.orElseWith(() => OptionEx.map2(toB(x), toB(y), P.B.equals))
    ->Option.getWithDefault(false)
}
