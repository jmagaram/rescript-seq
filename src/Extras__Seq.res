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

let fromArray = xs => {
  let rec go = index =>
    switch index >= xs->Js.Array2.length {
    | true => (. ()) => Empty
    | false => (. ()) => Next(xs->Js.Array2.unsafe_get(index), go(index + 1))
    }
  go(0)
}

let iterate = (seed, f) => unfold(seed, i => Some(i, f(i)))

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

let rec zipLongest = (s1, s2) => {
  (. ()) => {
    let s1 = s1(.)
    let s2 = s2(.)
    switch (s1, s2) {
    | (Empty, Empty) => Empty
    | (Next(value1, s1), Empty) => Next((Some(value1), None), zipLongest(s1, empty))
    | (Empty, Next(value2, s2)) => Next((None, Some(value2)), zipLongest(empty, s2))
    | (Next(value1, s1), Next(value2, s2)) => Next((Some(value1), Some(value2)), zipLongest(s1, s2))
    }
  }
}

let rec takeWhile = (seq, predicate) => {
  (. ()) => {
    switch seq(.) {
    | Empty => Empty
    | Next(value, seq) =>
      switch predicate(value) {
      | false => Empty
      | true => Next(value, takeWhile(seq, predicate))
      }
    }
  }
}

let rec zip = (seq1, seq2) =>
  (. ()) => {
    switch (seq1(.), seq2(.)) {
    | (Empty, _) => Empty
    | (_, Empty) => Empty
    | (Next(v1, s1), Next(v2, s2)) => Next((v1, v2), zip(s1, s2))
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

let reducei = (seq, zero, concat) =>
  seq->indexed->reduce(zero, (sum, (value, index)) => concat(~sum, ~value, ~index))

let toArray = seq =>
  seq->reduce([], (arr, i) => {
    arr->Js.Array2.push(i)->ignore
    arr
  })

let toReversedList = seq => seq->reduce(list{}, (lst, i) => lst->Belt.List.add(i))

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

let forEachi = (seq, f) => seq->indexed->forEach(((value, index)) => f(~value, ~index))

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

let findMap = (seq, f) => {
  let curr = ref(seq(.))
  let found = ref(None)
  while found.contents->Belt.Option.isNone && curr.contents !== Empty {
    switch curr.contents {
    | Empty => ()
    | Next(value, seq) => {
        switch f(value) {
        | None => ()
        | Some(_) as m => found := m
        }
        curr := seq(.)
      }
    }
  }
  found.contents
}

let find = (seq, predicate) => seq->findMap(i => predicate(i) ? Some(i) : None)

let equals = (s1: t<'a>, s2: t<'b>, eq) =>
  zipLongest(s1, s2)->everyOrEmpty(((a, b)) =>
    switch (a, b) {
    | (Some(a), Some(b)) => eq(a, b)
    | (None, None) => true
    | _ => false
    }
  )

let length = seq => seq->reduce(0, (sum, _) => sum + 1)
