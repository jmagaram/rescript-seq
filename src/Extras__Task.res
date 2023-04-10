module R = Belt.Result
module RX = Extras__Result
module Promise = Js.Promise2

@genType
type t<'a> = unit => promise<'a>

external toExn: 'a => exn = "%identity"

let make = (~promise, ~onError, ()) =>
  promise()->Promise.catch(e => e->toExn->onError->Promise.resolve)

let map = (t, f, ()) => t()->Promise.then(r => f(r)->Promise.resolve)

let toPromise = t => t()

let forEach = (t, f) => t->map(f)

let spy = (t, effect, ()) =>
  t()->Promise.then(r => {
    effect(r)
    r->Promise.resolve
  })

module Result = {
  let make = (~promise, ~onError) =>
    make(
      ~promise=() => promise()->Promise.then(ok => Ok(ok)->Promise.resolve),
      ~onError=e => Error(onError(e)),
    )

  let mapError = (t, f) => t->map(r => r->RX.mapError(f))

  let mapOk = (t, f) => t->map(r => r->R.map(f))

  let flatMap = (t, f) => t->map(r => r->R.flatMap(f))

  let toOption = t => t->map(RX.toOption)
}
