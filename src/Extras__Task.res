module Promise = Js.Promise2

type t<'a> = unit => promise<'a>

external toExn: 'a => exn = "%identity"

let makeNeverFail = (~promise, ~onError, ()) =>
  promise()->Promise.catch(e => e->toExn->onError->Promise.resolve)

let makeResult = (~promise, ~onError, ()) =>
  promise()
  ->Promise.then(ok => Ok(ok)->Promise.resolve)
  ->Promise.catch(err => err->toExn->onError->Error->Promise.resolve)

let map = (t, f, ()) => t()->Promise.then(r => f(r)->Promise.resolve)

let mapError = (t, f, ()) => t()->Promise.then(r => r->Extras__Result.mapError(f)->Promise.resolve)
let mapOk = (t, f, ()) => t()->Promise.then(r => r->Belt.Result.map(f)->Promise.resolve)

let toPromise = t => t()
