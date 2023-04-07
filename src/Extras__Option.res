open Belt

let isSomeAnd = (o, p) =>
  switch o {
  | None => false
  | Some(v) => p(v)
  }

let isNoneOr = (o, p) =>
  switch o {
  | None => true
  | Some(v) => p(v)
  }

let toArray = o => o->Option.mapWithDefault([], i => [i])

let concat = (x, y, f) =>
  switch (x, y) {
  | (x, None) => x
  | (None, y) => y
  | (Some(x), Some(y)) => Some(f(x, y))
  }

let fold = (x, f, sum) =>
  switch x {
  | None => sum
  | Some(x) => f(sum, x)
  }

let foldBack = (sum, f, x) =>
  switch x {
  | None => sum
  | Some(x) => f(x, sum)
  }

let fromOk = r =>
  switch r {
  | Ok(ok) => Some(ok)
  | _ => None
  }

let fromError = r =>
  switch r {
  | Ok(_) => None
  | Error(err) => Some(err)
  }

let fromTryCatch = f => {
  try {
    Some(f())
  } catch {
  | _ => None
  }
}

let map2 = (a, b, m) =>
  switch (a, b) {
  | (Some(a), Some(b)) => Some(m(a, b))
  | _ => None
  }

let orElseWith = (a, b) =>
  switch a {
  | Some(_) as a => a
  | _ => b()
  }
