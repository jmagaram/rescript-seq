type t<'a>
let makeInfallible: (~promise: unit => promise<'a>, ~onError: exn => 'a) => t<'a>
let make: (unit => promise<'a>) => t<result<'a, exn>>
let map: (t<'a>, 'a => 'b) => t<'b>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
let mapError: (t<result<'a, 'errA>>, 'errA => 'errB) => t<result<'a, 'errB>>
let toPromise: t<'a> => promise<'a>
