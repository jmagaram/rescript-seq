type t<'ok, 'err>
let make: (unit => promise<'ok>) => t<'ok, exn>
let map: (t<'okA, 'err>, 'okA => 'okB) => t<'okB, 'err>
let mapError: (t<'ok, 'errA>, 'errA => 'errB) => t<'ok, 'errB>
let toPromise: t<'ok, 'err> => promise<result<'ok, 'err>>
