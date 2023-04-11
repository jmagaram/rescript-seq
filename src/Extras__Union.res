module Option = Belt.Option
module OptionEx = Extras__Option
module Pattern = Extras__Pattern

module Make2 = (
  P: {
    module A: Pattern.T
    module B: Pattern.T
  },
) => {
  type t
  type a = P.A.t
  type b = P.B.t

  module A_Tools = Pattern.MakeTools(P.A)
  module B_Tools = Pattern.MakeTools(P.B)

  external fromA: a => t = "%identity"
  external fromB: b => t = "%identity"

  let toA = A_Tools.make
  let toB = B_Tools.make

  let make: 'a => option<t> = value =>
    value->A_Tools.make->Obj.magic->OptionEx.orElseWith(() => value->B_Tools.make->Obj.magic)

  let matchAB = (value, ~onA, ~onB) =>
    switch value
    ->A_Tools.make
    ->Option.map(onA)
    ->OptionEx.orElseWith(() => value->B_Tools.make->Option.map(onB)) {
    | Some(value) => value
    | None =>
      Js.Exn.raiseError("The value was unsafely cast and did not match any of the provided types.")
    }

  let equals = (x: t, y: t) =>
    A_Tools.eq(x, y)->OptionEx.orElseWith(() => B_Tools.eq(x, y))->Option.getWithDefault(false)
}

module Make3 = (
  P: {
    module A: Pattern.T
    module B: Pattern.T
    module C: Pattern.T
  },
) => {
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t

  module A_Tools = Pattern.MakeTools(P.A)
  module B_Tools = Pattern.MakeTools(P.B)
  module C_Tools = Pattern.MakeTools(P.C)

  external fromA: a => t = "%identity"
  external fromB: b => t = "%identity"
  external fromC: c => t = "%identity"

  let toA = A_Tools.make
  let toB = B_Tools.make
  let toC = C_Tools.make

  let make: 'a => option<t> = value =>
    value
    ->A_Tools.make
    ->Obj.magic
    ->OptionEx.orElseWith(() => value->B_Tools.make->Obj.magic)
    ->OptionEx.orElseWith(() => value->C_Tools.make->Obj.magic)

  let matchABC = (value, ~onA, ~onB, ~onC) =>
    switch value
    ->A_Tools.make
    ->Option.map(onA)
    ->OptionEx.orElseWith(() => value->B_Tools.make->Option.map(onB))
    ->OptionEx.orElseWith(() => value->C_Tools.make->Option.map(onC)) {
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

module Make4 = (
  P: {
    module A: Pattern.T
    module B: Pattern.T
    module C: Pattern.T
    module D: Pattern.T
  },
) => {
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t
  type d = P.D.t

  module A_Tools = Pattern.MakeTools(P.A)
  module B_Tools = Pattern.MakeTools(P.B)
  module C_Tools = Pattern.MakeTools(P.C)
  module D_Tools = Pattern.MakeTools(P.D)

  external fromA: a => t = "%identity"
  external fromB: b => t = "%identity"
  external fromC: c => t = "%identity"
  external fromD: d => t = "%identity"

  let toA = A_Tools.make
  let toB = B_Tools.make
  let toC = C_Tools.make
  let toD = D_Tools.make

  let make: 'a => option<t> = value =>
    value
    ->A_Tools.make
    ->Obj.magic
    ->OptionEx.orElseWith(() => value->B_Tools.make->Obj.magic)
    ->OptionEx.orElseWith(() => value->C_Tools.make->Obj.magic)
    ->OptionEx.orElseWith(() => value->D_Tools.make->Obj.magic)

  let matchABCD = (value, ~onA, ~onB, ~onC, ~onD) =>
    switch value
    ->A_Tools.make
    ->Option.map(onA)
    ->OptionEx.orElseWith(() => value->B_Tools.make->Option.map(onB))
    ->OptionEx.orElseWith(() => value->C_Tools.make->Option.map(onC))
    ->OptionEx.orElseWith(() => value->D_Tools.make->Option.map(onD)) {
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

module Make5 = (
  P: {
    module A: Pattern.T
    module B: Pattern.T
    module C: Pattern.T
    module D: Pattern.T
    module E: Pattern.T
  },
) => {
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t
  type d = P.D.t
  type e = P.E.t

  module A_Tools = Pattern.MakeTools(P.A)
  module B_Tools = Pattern.MakeTools(P.B)
  module C_Tools = Pattern.MakeTools(P.C)
  module D_Tools = Pattern.MakeTools(P.D)
  module E_Tools = Pattern.MakeTools(P.E)

  external fromA: a => t = "%identity"
  external fromB: b => t = "%identity"
  external fromC: c => t = "%identity"
  external fromD: d => t = "%identity"
  external fromE: e => t = "%identity"

  let toA = A_Tools.make
  let toB = B_Tools.make
  let toC = C_Tools.make
  let toD = D_Tools.make
  let toE = E_Tools.make

  let make: 'a => option<t> = value =>
    value
    ->A_Tools.make
    ->Obj.magic
    ->OptionEx.orElseWith(() => value->B_Tools.make->Obj.magic)
    ->OptionEx.orElseWith(() => value->C_Tools.make->Obj.magic)
    ->OptionEx.orElseWith(() => value->D_Tools.make->Obj.magic)
    ->OptionEx.orElseWith(() => value->E_Tools.make->Obj.magic)

  let matchABCDE = (value, ~onA, ~onB, ~onC, ~onD, ~onE) =>
    switch value
    ->A_Tools.make
    ->Option.map(onA)
    ->OptionEx.orElseWith(() => value->B_Tools.make->Option.map(onB))
    ->OptionEx.orElseWith(() => value->C_Tools.make->Option.map(onC))
    ->OptionEx.orElseWith(() => value->D_Tools.make->Option.map(onD))
    ->OptionEx.orElseWith(() => value->E_Tools.make->Option.map(onE)) {
    | Some(value) => value
    | None =>
      Js.Exn.raiseError("The value was unsafely cast and did not match any of the provided types.")
    }

  let equals = (x: t, y: t) =>
    A_Tools.eq(x, y)
    ->OptionEx.orElseWith(() => B_Tools.eq(x, y))
    ->OptionEx.orElseWith(() => C_Tools.eq(x, y))
    ->OptionEx.orElseWith(() => D_Tools.eq(x, y))
    ->OptionEx.orElseWith(() => E_Tools.eq(x, y))
    ->Option.getWithDefault(false)
}
