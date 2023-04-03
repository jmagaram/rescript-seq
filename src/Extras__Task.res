module Promise = Js.Promise2

type t<'a> = unit => promise<'a>

external toExn: 'a => exn = "%identity"

let make = (~promise, ~onError, ()) =>
  promise()->Promise.catch(e => e->toExn->onError->Promise.resolve)

let map = (t, f, ()) => t()->Promise.then(r => f(r)->Promise.resolve)

let toPromise = t => t()
