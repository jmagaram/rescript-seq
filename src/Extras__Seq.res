type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | Empty
  | Next('a, t<'a>)

// =========
// Construct
// =========

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

let infinite = f => unfold(0, _ => Some(f(), 0))

// =========
// Transform
// =========

let rec append = (s1: t<'a>, s2: t<'a>) => {
  (. ()) =>
    switch s1(.) {
    | Empty => s2(.)
    | Next(s1Value, b) => Next(s1Value, append(b, s2))
    }
}

let rec flatMap = (seq: t<'a>, f: 'a => t<'b>) => {
  (. ()) =>
    switch seq(.) {
    | Empty => Empty
    | Next(value, next) => append(f(value), flatMap(next, f))(.)
    }
}

let map = (seq, f) => flatMap(seq, i => singleton(f(i)))

let indexed = seq => {
  let rec go = (seq, index) =>
    (. ()) =>
      switch seq(.) {
      | Empty => Empty
      | Next(value, seq) => Next((value, index), go(seq, index + 1))
      }
  go(seq, 0)
}

let take = (seq, count) => {
  let rec go = seq => {
    switch seq(.) {
    | Empty => Empty
    | Next((value, index), seq) => index >= count ? Empty : Next(value, (. ()) => go(seq))
    }
  }
  (. ()) => go(seq->indexed)
}

let drop = (seq, count) => {
  let rec go = seq => {
    switch seq(.) {
    | Empty => Empty
    | Next((value, index), seq) =>
      index < count ? go(seq) : Next(value, seq->map(((value, _)) => value))
    }
  }
  (. ()) => go(seq->indexed)
}

let rec filter = (seq, f) => {
  (. ()) =>
    switch seq(.) {
    | Empty => Empty
    | Next(value, seq) =>
      switch f(value) {
      | true => Next(value, filter(seq, f))
      | false => filter(seq, f)(.)
      }
    }
}

// =======
// Consume
// =======

let reduce = (seq, zero, concat) => {
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
  seq->reduce([], (arr, i) => {
    arr->Js.Array2.push(i)->ignore
    arr
  })

let toReversedList = seq => seq->reduce(list{}, (lst, i) => lst->Belt.List.add(i))

let forEachi = (seq, f) => {
  let curr = ref(seq(.))
  let index = ref(0)
  while curr.contents !== Empty {
    switch curr.contents {
    | Empty => ()
    | Next(value, seq) => {
        f(~value, ~index=index.contents)
        index := index.contents + 1
        curr := seq(.)
      }
    }
  }
}

let forEach = (seq, f) => {
  let curr = ref(seq(.))
  while curr.contents !== Empty {
    switch curr.contents {
    | Empty => ()
    | Next(value, seq) => {
        f(value)
        curr := seq(.)
      }
    }
  }
}

let some = (seq, predicate) => {
  let break = ref(false)
  let curr = ref(seq(.))
  let result = ref(false)
  while result.contents !== true && curr.contents !== Empty {
    switch curr.contents {
    | Empty => break := true
    | Next(value, seq) => {
        result := predicate(value)
        curr := seq(.)
      }
    }
  }
  result.contents
}

let everyOrEmpty = (seq, predicate) => {
  let break = ref(false)
  let curr = ref(seq(.))
  let foundInvalid = ref(false)
  while foundInvalid.contents === false && curr.contents !== Empty {
    switch curr.contents {
    | Empty => break := true
    | Next(value, seq) => {
        foundInvalid := !predicate(value)
        curr := seq(.)
      }
    }
  }
  !foundInvalid.contents
}

let find = (seq, predicate) => {
  let curr = ref(seq(.))
  let found = ref(None)
  while found.contents->Belt.Option.isNone && curr.contents !== Empty {
    switch curr.contents {
    | Empty => ()
    | Next(value, seq) => {
        if predicate(value) {
          found := Some(value)
        }
        curr := seq(.)
      }
    }
  }
  found.contents
}

let length = seq => seq->reduce(0, (sum, _) => sum + 1)
