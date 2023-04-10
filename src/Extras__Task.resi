@genType.ignoreInterface
type t<'a>
let make: (~promise: unit => promise<'a>, ~onError: exn => 'a) => t<'a>
let map: (t<'a>, 'a => 'b) => t<'b>
let toPromise: t<'a> => promise<'a>
let forEach: (t<'a>, 'a => unit) => t<unit>
let spy: (t<'a>, 'a => unit) => t<'a>
