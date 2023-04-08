module Option = Belt.Option
module OptionEx = Extras__Option
module Unknown = Extras__Unknown

module type Pattern = {
  type t
  let isTypeOf: Unknown.t => bool
  let equals: (t, t) => bool
}

module IntPattern = {
  type t = int
  let isTypeOf = u => u->Unknown.typeof == #number
  let equals = (x: int, y: int) => x === y
}

module FloatPattern = {
  type t = float
  let isTypeOf = u => u->Unknown.typeof == #number
  let equals = (x: float, y: float) => x === y
}

module BoolPattern = {
  type t = bool
  let isTypeOf = u => u->Unknown.typeof == #boolean
  let equals = (x: bool, y: bool) => x === y
}

module StringPattern = {
  type t = string
  let isTypeOf = u => u->Unknown.typeof == #string
  let equals = (x: string, y: string) => Js.String2.localeCompare(x, y) == 0.0
}

// https://stackoverflow.com/questions/643782/how-to-check-whether-an-object-is-a-date
module DatePattern = {
  type t = Js.Date.t
  let isTypeOf: 'a => bool = %raw(`function (a) { return (!isNaN(a) && (a instanceof Date) && (typeof a.getMonth === 'function')) }`)
  let equals = (x: Js.Date.t, y: Js.Date.t) => x == y
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

  external fromA: a => t = "%identity"
  external fromB: b => t = "%identity"
  external fromC: c => t = "%identity"
  external fromD: d => t = "%identity"

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

  module A_Tools = PatternTools(P.A)
  module B_Tools = PatternTools(P.B)
  module C_Tools = PatternTools(P.C)

  external fromA: a => t = "%identity"
  external fromB: b => t = "%identity"
  external fromC: c => t = "%identity"

  let make = value =>
    value
    ->A_Tools.make
    ->Option.map(fromA)
    ->Option.orElse(value->B_Tools.make->Option.map(fromB))
    ->Option.orElse(value->C_Tools.make->Option.map(fromC))

  let match = (value, ~onA, ~onB, ~onC) =>
    switch value
    ->A_Tools.make
    ->Option.map(onA)
    ->Option.orElse(value->B_Tools.make->Option.map(onB))
    ->Option.orElse(value->C_Tools.make->Option.map(onC)) {
    | Some(value) => value
    | None =>
      Js.Exn.raiseError("The value was unsafely cast and did not match any of the provided types.")
    }

  let equals = (x: t, y: t) =>
    A_Tools.eq(x, y)
    ->OptionEx.orElseWith(() => B_Tools.eq(x, y))
    ->OptionEx.orElseWith(() => C_Tools.eq(x, y))
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

  module A_Tools = PatternTools(P.A)
  module B_Tools = PatternTools(P.B)

  external fromA: a => t = "%identity"
  external fromB: b => t = "%identity"

  let make = value =>
    value->A_Tools.make->Option.map(fromA)->Option.orElse(value->B_Tools.make->Option.map(fromB))

  let match = (value, ~onA, ~onB) =>
    switch value
    ->A_Tools.make
    ->Option.map(onA)
    ->Option.orElse(value->B_Tools.make->Option.map(onB)) {
    | Some(value) => value
    | None =>
      Js.Exn.raiseError("The value was unsafely cast and did not match any of the provided types.")
    }

  let equals = (x: t, y: t) =>
    A_Tools.eq(x, y)->OptionEx.orElseWith(() => B_Tools.eq(x, y))->Option.getWithDefault(false)
}
