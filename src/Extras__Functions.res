module Option = Belt.Option

module Trampoline = {
  type rec t<'a> =
    | Done('a)
    | Work(unit => t<'a>)

  let work = f => Work(f)

  let resolve = n => Done(n)

  let solve = t => {
    let state = ref(t)
    let solution = ref(None)
    while solution.contents->Option.isNone {
      switch state.contents {
      | Done(s) => solution := Some(s)
      | Work(f) => state := f()
      }
    }
    solution.contents->Option.getExn
  }
}
