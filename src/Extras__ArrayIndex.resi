type t
external toInt: t => int = "%identity"
external make: int => t = "%identity"
let toOption: t => option<int>
let match: (t, ~isValid: int => 'a, ~notFound: unit => 'a) => 'a

// needed? if doing this probably want to get the last item instead
let last: array<'a> => t
let first: array<'a> => t
