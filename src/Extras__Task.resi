@genType.ignoreInterface
type t<'a>
let make: (~promise: unit => promise<'a>, ~onError: exn => 'a) => t<'a>
let map: (t<'a>, 'a => 'b) => t<'b>
let toPromise: t<'a> => promise<'a>
let forEach: (t<'a>, 'a => unit) => t<unit>
let spy: (t<'a>, 'a => unit) => t<'a>

module Result: {
  let make: (~promise: unit => promise<'a>, ~onError: exn => 'b) => t<result<'a, 'b>>
  let mapError: (t<result<'a, 'errA>>, 'errA => 'errB) => t<result<'a, 'errB>>
  let mapOk: (t<result<'a, 'err>>, 'a => 'b) => t<result<'b, 'err>>
  let flatMap: (t<result<'a, 'err>>, 'a => result<'b, 'err>) => t<result<'b, 'err>>
  let toOption: t<result<'a, 'err>> => t<option<'a>>
}
