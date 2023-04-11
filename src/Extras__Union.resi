/**
This module provides functors to create tagged or untagged unions of 2, 3, 4, or 5 items. Here is a
simple one. This creates a module that includes the type StringOrFalse.t, safe constructors, pattern 
matching, and a custom equality function. You can build unions using any types, and with unlimited
ability to discriminate using typeof, instanceof, or a JSON parsing library.

module StringOrFalse = Union.Make2({
    module A = Union.Patterns.String
    module B = Literal.False
})

Each case in a union is defined by a module conforming to `Pattern` (see module), which defines
a type and a parsing function, which is kind of like a type guard in TypeScript. Patterns can
define ordinary types as well as literal values.

For more details:
- README at https://github.com/jmagaram/rescript-extras 
- Examples https://github.com/jmagaram/rescript-extras/blob/master/tests/Extras__UnionTests.res
- Patterns https://github.com/jmagaram/rescript-extras/blob/master/Extras__Pattern.resi 
- Literals https://github.com/jmagaram/rescript-extras/blob/master/Extras__Literal.resi 

*/
module Make2: (
  P: {
    module A: Extras__Pattern.T
    module B: Extras__Pattern.T
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
    let matchAB: (t, ~onA: a => 'x, ~onB: b => 'x) => 'x
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
)

module Make3: (
  P: {
    module A: Extras__Pattern.T
    module B: Extras__Pattern.T
    module C: Extras__Pattern.T
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
    let matchABC: (t, ~onA: a => 'x, ~onB: b => 'x, ~onC: c => 'x) => 'x
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
    and type c = P.C.t
)

module Make4: (
  P: {
    module A: Extras__Pattern.T
    module B: Extras__Pattern.T
    module C: Extras__Pattern.T
    module D: Extras__Pattern.T
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
    let matchABCD: (t, ~onA: a => 'x, ~onB: b => 'x, ~onC: c => 'x, ~onD: d => 'x) => 'x
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
    and type c = P.C.t
    and type d = P.D.t
)

module Make5: (
  P: {
    module A: Extras__Pattern.T
    module B: Extras__Pattern.T
    module C: Extras__Pattern.T
    module D: Extras__Pattern.T
    module E: Extras__Pattern.T
  },
) =>
(
  {
    type t
    type a
    type b
    type c
    type d
    type e

    external fromA: a => t = "%identity"
    external fromB: b => t = "%identity"
    external fromC: c => t = "%identity"
    external fromD: d => t = "%identity"
    external fromE: e => t = "%identity"

    let toA: t => option<a>
    let toB: t => option<b>
    let toC: t => option<c>
    let toD: t => option<d>
    let toE: t => option<e>

    let make: 'a => option<t>
    let matchABCDE: (
      t,
      ~onA: a => 'x,
      ~onB: b => 'x,
      ~onC: c => 'x,
      ~onD: d => 'x,
      ~onE: e => 'x,
    ) => 'x
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
    and type c = P.C.t
    and type d = P.D.t
    and type e = P.E.t
)
