type t
external toInt: t => int = "%identity"
external make: int => t = "%identity"
let toOption: t => option<int>
let match: (t, ~isValid: int => 'a, ~notFound: unit => 'a) => 'a
