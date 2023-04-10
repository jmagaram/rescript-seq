module Promise = Js.Promise2
module Task = Extras__Task
module Result = Belt.Result

type t<'ok, 'err> = Task.t<result<'ok, 'err>>

let make = (~promise, ~onError) =>
  Task.make(
    ~promise=() => promise()->Promise.then(ok => Ok(ok)->Promise.resolve),
    ~onError=e => Error(onError(e)),
  )

let mapError = (t, f) => t->Task.map(r => r->Extras__Result.mapError(f))
let mapOk = (t, f) => t->Task.map(r => r->Result.map(f))
let mapBoth = (t, ~ok, ~error) =>
  t->Task.map(r =>
    switch r {
    | Ok(i) => ok(i)
    | Error(i) => error(i)
    }
  )
let flatMap = (t, f) => t->Task.map(r => r->Result.flatMap(f))

let toPromise = t => t->Task.toPromise
