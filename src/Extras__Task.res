module Promise = Js.Promise2

type t<'a> = unit => promise<'a>

external toExn: 'a => exn = "%identity"

let makeInfallible = (~promise, ~onError, ()) =>
  promise()->Promise.catch(e => e->toExn->onError->Promise.resolve)

let make = (promise, ()) => {
  promise()
  ->Promise.then(ok => Ok(ok)->Promise.resolve)
  ->Promise.catch(err => err->toExn->Error->Promise.resolve)
}

let map = (t, f, ()) => t()->Promise.then(r => f(r)->Promise.resolve)

let mapError = (t, f, ()) => t()->Promise.then(r => r->Extras__Result.mapError(f)->Promise.resolve)

let flatMap = (t, m, ()) => t()->Promise.then(r => m(r)())

let toPromise = t => t()
