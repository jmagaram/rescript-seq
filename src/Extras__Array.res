module Array = Belt.Array

let push = Js.Array2.push

let fromSeed = (state, generator) => {
  let result = []
  let state = ref(state)
  let break = ref(false)
  while !break.contents {
    switch generator(state.contents) {
    | None => break := true
    | Some((item, nextState)) => {
        result->push(item)->ignore
        state := nextState
      }
    }
  }
  result
}

@val external fromOneValue: 'a => array<'a> = "Array.of"

let fromOption = opt =>
  switch opt {
  | None => []
  | Some(i) => fromOneValue(i)
  }

let isEmpty = xs => xs->Array.length == 0
let isNotEmpty = xs => xs->Array.length > 0

let exactlyOneValue = xs =>
  switch xs->Array.length {
  | 1 => xs->Js.Array2.unsafe_get(0)->Some
  | _ => None
  }

let head = xs => xs->Array.get(0)

let last = xs =>
  switch xs->isEmpty {
  | true => None
  | false => xs->Array.get(xs->Array.length - 1)
  }

let lastIndex = xs => xs->Array.length->(i => i == 0 ? None : Some(i - 1))

let pairs = xs => {
  switch xs->lastIndex {
  | None
  | Some(0) => []
  | Some(last) =>
    Array.init(last, i => (xs->Js.Array2.unsafe_get(i), xs->Js.Array2.unsafe_get(i + 1)))
  }
}

let prepend = (a, b) => Js.Array2.concat(b, a)

let filterSomeWith = (xs, f) => {
  let result = []
  for i in 0 to Js.Array2.length(xs) - 1 {
    let item = xs->Js.Array2.unsafe_get(i)
    switch f(item, i) {
    | None => ()
    | Some(i) => result->Js.Array2.push(i)->ignore
    }
  }
  result
}

let filterSome = xs => xs->filterSomeWith((value, _) => value)
