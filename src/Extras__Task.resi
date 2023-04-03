type t<'a>
let makeNeverFail: (~promise: unit => promise<'a>, ~onError: exn => 'a) => t<'a>
let makeResult: (~promise: unit => promise<'a>, ~onError: exn => 'b) => t<result<'a, 'b>>
let map: (t<'a>, 'a => 'b) => t<'b>
let mapError: (t<result<'a, 'errA>>, 'errA => 'errB) => t<result<'a, 'errB>>
let mapOk: (t<result<'a, 'err>>, 'a => 'b) => t<result<'b, 'err>>
let toPromise: t<'a> => promise<'a>
