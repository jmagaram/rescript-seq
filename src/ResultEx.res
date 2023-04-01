open Belt

let fromArrayMap = (xs, f) => {
  let oks = []
  let rec go = inx =>
    switch xs->Array.get(inx) {
    | None => Ok(oks)
    | Some(x) =>
      switch f(x) {
      | Ok(ok) =>
        oks->Array.push(ok)
        go(inx + 1)
      | Error(_) as err => err
      }
    }
  go(0)
}

let fromArray = xs => xs->fromArrayMap(i => i)

let mapError = (r, f) =>
  switch r {
  | Ok(_) as ok => ok
  | Error(e) => Error(f(e))
  }

let fromTryCatchWith = (f, m) => {
  try {
    let ok = f()
    Ok(ok)
  } catch {
  | _ as e => Error(m(e))
  }
}

let fromTryCatch = f => fromTryCatchWith(f, i => i)

let ok = r =>
  switch r {
  | Ok(ok) => Some(ok)
  | Error(_) => None
  }

let error = r =>
  switch r {
  | Ok(_) => None
  | Error(e) => Some(e)
  }
