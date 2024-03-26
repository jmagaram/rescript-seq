type t<'a> = unit => promise<'a>

let make = (~promise, ~onError) => () => promise()->Promise.catch(e => e->onError->Promise.resolve)

let map = (t, f) => () => t()->Promise.then(r => f(r)->Promise.resolve)

let toPromise = t => t()

let forEach = (t, f) => t->map(f)

let spy = (t, effect) => () =>
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

  let mapError = (t, f) => () =>
    t()->Promise.then(r =>
      switch r {
      | Ok(_) as ok => ok
      | Error(err) => Error(f(err))
      }->Promise.resolve
    )

  let mapOk = (t, f) => () =>
    t()->Promise.then(r =>
      switch r {
      | Ok(ok) => Ok(f(ok))
      | Error(_) as err => err
      }->Promise.resolve
    )

  let flatMap = (t, f) => () =>
    t()->Promise.then(r =>
      switch r {
      | Ok(ok) => f(ok)
      | Error(_) as err => err
      }->Promise.resolve
    )

  let toOption = t => () =>
    t()->Promise.then(r =>
      switch r {
      | Ok(ok) => Some(ok)
      | Error(_) => None
      }->Promise.resolve
    )
}
