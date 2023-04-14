type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | Empty
  | Next('a, t<'a>)

let empty = (. ()) => Empty

let singleton = v => (. ()) => Next(v, empty)

let fold = (seq, zero, concat) => {
  let sum = ref(zero)
  let curr = ref(seq(.))
  while curr.contents !== Empty {
    switch curr.contents {
    | Empty => ()
    | Next(v, seq) => {
        curr := seq(.)
        sum := concat(sum.contents, v)
      }
    }
  }
  sum.contents
}

let toArray = seq =>
  seq->fold([], (arr, i) => {
    arr->Js.Array2.push(i)->ignore
    arr
  })
