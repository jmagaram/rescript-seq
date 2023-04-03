type t = int
external make: int => t = "%identity"
external toInt: t => int = "%identity"
let toOption = i => i < 0 ? None : Some(i)
let match = (i, ~isValid, ~notFound) => i >= 0 ? isValid(i) : notFound()

let last = xs =>
  switch xs->Array.length {
  | 0 => -1
  | len => len - 1
  }
let first = xs =>
  switch xs->Array.length {
  | 0 => -1
  | _ => 0
  }
