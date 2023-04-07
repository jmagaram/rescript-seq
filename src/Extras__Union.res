module Option = Belt.Option
module OptionEx = Extras__Option
module Unknown = Extras__Unknown

module type Variant = {
  type t
  let isTypeOf: Unknown.t => bool // Accept 'a without error?
  let equals: (t, t) => bool
}

module Make4 = (
  P: {
    module A: Variant
    module B: Variant
    module C: Variant
    module D: Variant
  },
) => {
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t
  type d = P.D.t

  let toA = value => P.A.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): a)) : None
  let toB = value => P.B.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): b)) : None
  let toC = value => P.C.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): c)) : None
  let toD = value => P.D.isTypeOf(value->Unknown.make) ? Some((Obj.magic(value): d)) : None

  let fromA = (value: a): t => Obj.magic(value)
  let fromB = (value: b): t => Obj.magic(value)
  let fromC = (value: c): t => Obj.magic(value)
  let fromD = (value: d): t => Obj.magic(value)

  let make = value =>
    value
    ->toA
    ->Option.map(fromA)
    ->Option.orElse(value->toB->Option.map(fromB))
    ->Option.orElse(value->toC->Option.map(fromC))
    ->Option.orElse(value->toD->Option.map(fromD))

  let match = (value, ~onA, ~onB, ~onC, ~onD) =>
    switch value
    ->toA
    ->Option.map(onA)
    ->Option.orElse(value->toB->Option.map(onB))
    ->Option.orElse(value->toC->Option.map(onC))
    ->Option.orElse(value->toD->Option.map(onD)) {
    | Some(value) => value
    | None =>
      Js.Exn.raiseError("The value was unsafely cast and did not match any of the provided types.")
    }

  let equals = (x: t, y: t) =>
    OptionEx.map2(toA(x), toA(y), P.A.equals)
    ->OptionEx.orElseWith(() => OptionEx.map2(toB(x), toB(y), P.B.equals))
    ->OptionEx.orElseWith(() => OptionEx.map2(toC(x), toC(y), P.C.equals))
    ->OptionEx.orElseWith(() => OptionEx.map2(toD(x), toD(y), P.D.equals))
    ->Option.getWithDefault(false)
}

module Make3 = (
  P: {
    module A: Variant
    module B: Variant
    module C: Variant
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
    module A: Variant
    module B: Variant
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
