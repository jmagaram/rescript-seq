type t<'a>
let make: (~promise: unit => promise<'a>, ~onError: exn => 'a) => t<'a>
let map: (t<'a>, 'a => 'b) => t<'b>
let toPromise: t<'a> => promise<'a>
