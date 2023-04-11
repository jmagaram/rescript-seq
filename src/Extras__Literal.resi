module type T = {
  type t
  type domain
  let isTypeOf: unknown => bool
  let parse: 'a => option<t>
  let equals: (t, t) => bool
  let value: t
  external unwrap: t => domain = "%identity"
}

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
