module Task = Extras__Task

type t<'ok, 'err> = Task.t<result<'ok, 'err>>

let make: (~promise: unit => promise<'a>, ~onError: exn => 'b) => t<'a, 'b>

let mapError: (t<'a, 'errA>, 'errA => 'errB) => t<'a, 'errB>
let mapOk: (t<'a, 'err>, 'a => 'b) => t<'b, 'err>
let flatMap: (t<'a, 'err>, 'a => result<'b, 'err>) => t<'b, 'err>

let toPromise: t<'a, 'err> => promise<result<'a, 'err>>
