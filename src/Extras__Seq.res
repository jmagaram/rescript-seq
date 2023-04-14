type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | Empty
  | Next('a, t<'a>)

// Construct sequences

let empty = (. ()) => Empty

let singleton = v => (. ()) => Next(v, empty)

let rec unfold = (seed, f) =>
  (. ()) => {
    switch f(seed) {
    | None => Empty
    | Some(value, seed) => Next(value, unfold(seed, f))
    }
  }

let init = (~count, ~initializer) =>
  unfold(0, i => i < count ? Some(initializer(~index=i), i + 1) : None)

let replicate = (~count, ~value) => unfold(0, i => i < count ? Some(value, i + 1) : None)

// Transforming

// let flatMap2 = (seq, f) => {
//   let first = seq(.)
//   switch first {
//     | Empty =>
//   }
// }

let flatMap = (_seq, _f) => empty

// Consuming sequences

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

let toReversedList = seq => seq->fold(list{}, (lst, i) => lst->Belt.List.add(i))
