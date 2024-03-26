let fromTryCatchWith = (f, m) => {
  try {
    let ok = f()
    Ok(ok)
  } catch {
  | _ as e => Error(m(e))
  }
}

let fromTryCatch = f => fromTryCatchWith(f, i => i)

let toOption = r =>
  switch r {
  | Ok(ok) => Some(ok)
  | Error(_) => None
  }
