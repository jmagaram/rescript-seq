/** 
A module of type `Literal.T` defines a type with a single value, like -1, "yes",
and true. These are particular useful when defining unions. For example, a
tagged union might have a "shape" property with values for "circle" | "square".
Using a literal type provides more type safety than just a `string`.
*/
module type T = {
  type t
  type domain
  let isTypeOf: unknown => bool
  let parse: 'a => option<t>
  let equals: (t, t) => bool
  let value: t
  external unwrap: t => domain = "%identity"
}

/** 
Give a specific type and value, constructs a Literal module for that value.
**Note: ** This functor only works for immutable values where membership in the
type is tested with the strict equality === operator.
*/
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
