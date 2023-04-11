// Each choice in a union is represented by a module of this type.
module type T = {
  // The type can be a primitive like a string or int, an array, an object, or
  // basically anything.
  type t

  // The `isTypeOf` function determines whether a specific value can safely be
  // assigned to type `t`. Use any critieria such as `typeof`, `instanceof`,
  // lightly inspecting the object for various properties, or a JSON parser.
  // This functions like a TypeScript type guard.
  let isTypeOf: unknown => bool

  // Determines whether two instances of the type are equal. Can provide
  // optimized behavior over a deep recursive compare.
  let equals: (t, t) => bool
}

module MakeTools: (P: T) =>
{
  // Given any value, tries to create an instance of the type defined by the
  // pattern.
  let make: 'a => option<P.t>

  // Determines if two values are the equal according to the `equals` function
  // in the pattern. Returns `None` if neither parameter matches the type
  // defined by the pattern.
  let eq: ('a, 'b) => option<bool>
}

// Pattern matching for basic types. Literals built with the Literal module,
// such as `true`, `null`, `-1`, and `"yes"`, are also patterns that can
// participate in a union.
module Int: T with type t = int
module Float: T with type t = float
module Bool: T with type t = bool
module String: T with type t = string
module Date: T with type t = Js.Date.t
