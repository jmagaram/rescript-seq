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
