// Defines a type with a single value, like -1, "yes", and true. These are
// particular useful when defining unions. For example, a tagged union might
// have a "shape" property with values for "circle" | "square". These can't be
// any strings - they must be one of the predefined values.
module type T = {
  type t
  type domain
  let isTypeOf: unknown => bool
  let parse: 'a => option<t>
  let equals: (t, t) => bool
  let value: t
  external unwrap: t => domain = "%identity"
}

// Give a specific type and value, constructs a Literal module for that value.
// This functor only works for immutable values where membership in the type is
// tested with the strict equality === operator.
module Make: (
  C: {
    type domain
    let value: domain
  },
) => (T with type domain := C.domain)

module MakeInt: (
  C: {
    let value: int
  },
) => (T with type domain := int)

module MakeString: (
  C: {
    let value: string
    let trimmed: bool
    let caseInsensitive: bool
  },
) => (T with type domain := string)

module Null: T with type domain := Js.Null.t<unknown>
module Undefined: T with type domain := Js.Undefined.t<unknown>
module True: T with type domain := bool
module False: T with type domain := bool
