module type Variant = {
  type t
  let isTypeOf: Extras__Unknown.t => bool
  let equals: (t, t) => bool
}

module Make4: (
  P: {
    module A: Variant
    module B: Variant
    module C: Variant
    module D: Variant
  },
) =>
{
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t
  type d = P.D.t
  let toA: 'a => option<a>
  let toB: 'a => option<b>
  let toC: 'a => option<c>
  let toD: 'a => option<d>
  let fromA: a => t
  let fromB: b => t
  let fromC: c => t
  let fromD: d => t
  let make: 'a => option<t>
  let match: ('a, ~onA: a => 'res, ~onB: b => 'res, ~onC: c => 'res, ~onD: d => 'res) => 'res
  let equals: (t, t) => bool
}

module Make3: (
  P: {
    module A: Variant
    module B: Variant
    module C: Variant
  },
) =>
{
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t
  let toA: 'a => option<a>
  let toB: 'a => option<b>
  let toC: 'a => option<c>
  let fromA: a => t
  let fromB: b => t
  let fromC: c => t
  let make: 'a => option<t>
  let match: ('a, ~onA: a => 'res, ~onB: b => 'res, ~onC: c => 'res) => 'res
  let equals: (t, t) => bool
}
module Make2: (
  P: {
    module A: Variant
    module B: Variant
  },
) =>
{
  type t
  type a = P.A.t
  type b = P.B.t
  let toA: 'a => option<a>
  let toB: 'a => option<b>
  let fromA: a => t
  let fromB: b => t
  let make: 'a => option<t>
  let match: ('a, ~onA: a => 'res, ~onB: b => 'res) => 'res
  let equals: (t, t) => bool
}
