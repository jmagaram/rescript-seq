module Promise = Js.Promise2

type t<'ok, 'error> = {
  promise: unit => promise<'ok>,
  onError: exn => 'error,
}

let make = p => {promise: p, onError: i => i}

let map = (t, f) => {
  ...t,
  promise: () => t.promise()->Promise.then(i => f(i)->Promise.resolve),
}

let mapError = (t, m) => {
  ...t,
  onError: exn => exn->t.onError->m,
}

external toExn: Js.Promise2.error => exn = "%identity"

let toResult = t =>
  t.promise()
  ->Promise.then(i => Ok(i)->Promise.resolve)
  ->Promise.catch(e => e->toExn->t.onError->Error->Promise.resolve)
