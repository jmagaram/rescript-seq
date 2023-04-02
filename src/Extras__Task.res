module Result = Belt.Result
module Promise = Js.Promise2

type t<'ok, 'error> = unit => promise<result<'ok, 'error>>

external toExn: Promise.error => exn = "%identity"

let make = (p, ()) =>
  p()
  ->Promise.then(r => Ok(r)->Promise.resolve)
  ->Promise.catch(e => e->toExn->Error->Promise.resolve)

let map = (t, f, ()) => t()->Promise.then(r => r->Result.map(f)->Promise.resolve)

let mapError = (t, f, ()) => t()->Promise.then(r => r->Extras__Result.mapError(f)->Promise.resolve)

let toPromise = t => t()
