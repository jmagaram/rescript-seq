// This module provides functors to create tagged or untagged unions of 2, 3, or
// 4 items. Each union has a custom equality operator and functions for creation
// and pattern matching. There are alternatives to achieve a similar thing, such
// as using a JSON parsing library like rescript-struct or the the untagged
// union support built into the compiler. For more details, see the README at
// https://github.com/jmagaram/rescript-extras and consult the examples in
// https://github.com/jmagaram/rescript-extras/blob/master/src/Extras__UnionTests.res.

// Defines a single case in an untagged union.
module type Pattern = {
  // The underlying type. Can be a primitive like `string`, a reference type
  // like a JavaScript error, an array, or basically anything.
  type t

  // The `isTypeOf` function determines whether a specific value can safely be
  // assigned to type `t`. Use any critieria such as `typeof`, `instanceof`, or
  // inspecting the properties of the object.
  let isTypeOf: Extras__Unknown.t => bool

  // Determines whether two instances of the type are equal. Can provide
  // optimized behavior over a deep recursive compare.
  let equals: (t, t) => bool
}

// Pattern matching basic types. Literal types built with the Literal module,
// such as `true`, `null`, `-1`, and `"yes"`, are also patterns that can
// participate in a union.
module Patterns: {
  module Int: Pattern with type t = int
  module Float: Pattern with type t = float
  module Bool: Pattern with type t = bool
  module String: Pattern with type t = string
  module Date: Pattern with type t = Js.Date.t
}

// Make a union of 2 types
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
    let matchAB: (t, ~onA: a => 'x, ~onB: b => 'x) => 'x
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
)

// Make a union of 3 types
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
    let matchABC: (t, ~onA: a => 'x, ~onB: b => 'x, ~onC: c => 'x) => 'x
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
    and type c = P.C.t
)

// Make a union of 4 types
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
    let matchABCD: (t, ~onA: a => 'x, ~onB: b => 'x, ~onC: c => 'x, ~onD: d => 'x) => 'x
    let equals: (t, t) => bool
  }
    with type a = P.A.t
    and type b = P.B.t
    and type c = P.C.t
    and type d = P.D.t
)
