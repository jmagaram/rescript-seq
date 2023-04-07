module type Pattern = {
  type t
  let isTypeOf: Extras__Unknown.t => bool
  let equals: (t, t) => bool
}

module Make4: (
  P: {
    module A: Pattern
    module B: Pattern
    module C: Pattern
    module D: Pattern
  },
) =>
{
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t
  type d = P.D.t
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
    module A: Pattern
    module B: Pattern
    module C: Pattern
  },
) =>
{
  type t
  type a = P.A.t
  type b = P.B.t
  type c = P.C.t
  let fromA: a => t
  let fromB: b => t
  let fromC: c => t
  let make: 'a => option<t>
  let match: ('a, ~onA: a => 'res, ~onB: b => 'res, ~onC: c => 'res) => 'res
  let equals: (t, t) => bool
}
module Make2: (
  P: {
    module A: Pattern
    module B: Pattern
  },
) =>
{
  type t
  type a = P.A.t
  type b = P.B.t
  let fromA: a => t
  let fromB: b => t
  let make: 'a => option<t>
  let match: ('a, ~onA: a => 'res, ~onB: b => 'res) => 'res
  let equals: (t, t) => bool
}
