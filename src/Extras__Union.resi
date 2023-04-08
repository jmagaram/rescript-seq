module type Pattern = {
  type t
  let isTypeOf: Extras__Unknown.t => bool
  let equals: (t, t) => bool
}

module IntPattern: Pattern with type t = int
module FloatPattern: Pattern with type t = float
module BoolPattern: Pattern with type t = bool
module StringPattern: Pattern with type t = string
module DatePattern: Pattern with type t = Js.Date.t

module Make2: (
  P: {
    module A: Pattern
    module B: Pattern
  },
) =>
(
  {
    type t
    type a
    type b

    external fromA: a => t = "%identity"
    external fromB: b => t = "%identity"

    let toA: t => option<a>
    let toB: t => option<b>

    let make: 'a => option<t>
    let matchAB: (t, ~onA: a => 'res, ~onB: b => 'res) => 'res
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
)

module Make3: (
  P: {
    module A: Pattern
    module B: Pattern
    module C: Pattern
  },
) =>
(
  {
    type t
    type a
    type b
    type c

    external fromA: a => t = "%identity"
    external fromB: b => t = "%identity"
    external fromC: c => t = "%identity"

    let toA: t => option<a>
    let toB: t => option<b>
    let toC: t => option<c>

    let make: 'a => option<t>
    let matchABC: (t, ~onA: a => 'res, ~onB: b => 'res, ~onC: c => 'res) => 'res
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
    and type c = P.C.t
)

module Make4: (
  P: {
    module A: Pattern
    module B: Pattern
    module C: Pattern
    module D: Pattern
  },
) =>
(
  {
    type t
    type a
    type b
    type c
    type d

    external fromA: a => t = "%identity"
    external fromB: b => t = "%identity"
    external fromC: c => t = "%identity"
    external fromD: d => t = "%identity"

    let toA: t => option<a>
    let toB: t => option<b>
    let toC: t => option<c>
    let toD: t => option<d>

    let make: 'a => option<t>
    let matchABCD: (t, ~onA: a => 'res, ~onB: b => 'res, ~onC: c => 'res, ~onD: d => 'res) => 'res
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
    and type c = P.C.t
    and type d = P.D.t
)
