type t = int
external make: int => t = "%identity"
external toInt: t => int = "%identity"
let toOption = i => i < 0 ? None : Some(i)
let match = (i, ~isValid, ~notFound) => i >= 0 ? isValid(i) : notFound()
