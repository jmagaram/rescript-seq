module Option = Belt.Option
module Ex = Extras

type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | Empty
  | Next('a, t<'a>)

exception ArgumentOfOfRange(string)

let toOption = node =>
  switch node {
  | Empty => None
  | Next(value, seq) => Some(value, seq)
  }

type mapNext<'a, 'b> = (t<'a>, (~value: 'a, ~seq: t<'a>) => node<'b>) => t<'b>
let mapNext: mapNext<'a, 'b> = (seq, f) =>
  (. ()) =>
    switch seq(.) {
    | Empty => Empty
    | Next(value, seq) => f(~value, ~seq)
    }

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

let iterate = (seed, f) => unfold(seed, i => Some(i, f(i)))

let startWith = (seq, value) => (. ()) => Next(value, seq)

let rec append = (s1, s2) => {
  (. ()) =>
    switch s1(.) {
    | Empty => s2(.)
    | Next(s1Value, b) => Next(s1Value, append(b, s2))
    }
}

let startWithMany = (s1, s2) => append(s2, s1)

let range = (~start, ~stop) => {
  start < stop
    ? unfold(start, i => i <= stop ? Some(i, i + 1) : None)
    : unfold(start, i => i >= stop ? Some(i, i - 1) : None)
}

let rec tap = (seq, f) =>
  seq->mapNext((~value, ~seq) => {
    f(value)
    Next(value, tap(seq, f))
  })

let rec flatMap = (seq, f) => {
  (. ()) =>
    switch seq(.) {
    | Empty => Empty
    | Next(value, next) => append(f(value), flatMap(next, f))(.)
    }
}

let flatten = seq => seq->flatMap(i => i)

let cycle = seq => {
  (. ()) =>
    switch seq(.) {
    | Empty => Empty
    | Next(head, tail) => Next(head, append(tail, infinite(() => seq)->flatten))
    }
}

let appendMany = (s1, others) => s1->append(others->flatten)

let map = (seq, f) => flatMap(seq, i => singleton(f(i)))

let fromString = s =>
  switch s->Js.String2.length {
  | 0 => empty
  | len => range(~start=0, ~stop=len - 1)->map(inx => s->Js.String2.charAt(inx))
  }

let fromArray = (~start=?, ~end=?, arr: array<'a>) => {
  switch arr->Ex.Array.isEmpty {
  | true => empty
  | false => {
      let start = start->Option.getWithDefault(0)
      let end = end->Option.getWithDefault(arr->Ex.Array.lastIndex->Option.getUnsafe)
      range(~start, ~stop=end)->map(inx => arr->Js.Array2.unsafe_get(inx))
    }
  }
}

let rec fromList = xs => {
  (. ()) =>
    switch xs {
    | list{} => Empty
    | list{head, ...tail} => Next(head, fromList(tail))
    }
}

let fromOption = opt =>
  switch opt {
  | None => empty
  | Some(value) => singleton(value)
  }

let indexed = seq => {
  let rec go = (seq, index) =>
    seq->mapNext((~value, ~seq) => Next((value, index), go(seq, index + 1)))
  go(seq, 0)
}

let mapi = (seq, f) => seq->indexed->map(((value, index)) => f(~value, ~index))

let takeAtMost = (seq, count) => {
  let rec go = seq =>
    seq->mapNext((~value as (value, index), ~seq) =>
      switch index >= count {
      | true => Empty
      | false => Next(value, go(seq))
      }
    )
  go(seq->indexed)
}

let drop = (seq, count) => {
  let rec go = seq =>
    seq->mapNext((~value as (value, index), ~seq) =>
      switch index < count {
      | true => go(seq)(.)
      | false => Next(value, seq->map(((value, _)) => value))
      }
    )
  go(seq->indexed)
}

let filteri = (seq, f) => {
  let rec go = seq =>
    seq->mapNext((~value as (value, index), ~seq) =>
      switch f(~value, ~index) {
      | true => Next(value, go(seq))
      | false => go(seq)(.)
      }
    )
  go(seq->indexed)
}

let rec filter = (seq, f) =>
  seq->mapNext((~value, ~seq) =>
    switch f(value) {
    | true => Next(value, filter(seq, f))
    | false => filter(seq, f)(.)
    }
  )

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

let rec takeWhile = (seq, predicate) =>
  seq->mapNext((~value, ~seq) =>
    switch predicate(value) {
    | false => Empty
    | true => Next(value, takeWhile(seq, predicate))
    }
  )

let rec zip = (seq1, seq2) =>
  (. ()) => {
    switch (seq1(.), seq2(.)) {
    | (Empty, _) => Empty
    | (_, Empty) => Empty
    | (Next(v1, s1), Next(v2, s2)) => Next((v1, v2), zip(s1, s2))
    }
  }

let rec filterMap = (seq, f) =>
  (. ()) => {
    switch seq(.) {
    | Empty => Empty
    | Next(value, seq) =>
      switch f(value) {
      | None => filterMap(seq, f)(.) // recurses till finds
      | Some(value) => Next(value, filterMap(seq, f))
      }
    }
  }

let filterSome = seq => seq->filterMap(i => i)

let filterOk = seq =>
  seq->filterMap(i =>
    switch i {
    | Ok(ok) => Some(ok)
    | Error(_) => None
    }
  )

let scani = (seq, ~zero, f) => {
  let rec go = (seq, sum) =>
    switch seq(.) {
    | Empty => (. ()) => Empty
    | Next((value, index), seq) =>
      (. ()) => {
        let sum = f(~sum, ~value, ~index)
        Next(sum, go(seq, sum))
      }
    }
  append(singleton(zero), go(seq->indexed, zero))
  // not tail?
  // look at ocaml code
  // cons?
}

let rec dropWhile = (seq, predicate) => {
  (. ()) => {
    switch seq(.) {
    | Empty => Empty
    | Next(value, seq) =>
      switch predicate(value) {
      | true => dropWhile(seq, predicate)(.) // recursive!
      | false => Next(value, seq)
      }
    }
  }
}

let map2 = (s1, s2, f) => zip(s1, s2)->map(((a, b)) => f(a, b))

let rec sortedMerge = (s1, s2, cmp) => {
  (. ()) =>
    switch (s1(.), s2(.)) {
    | (Empty, Next(_, _) as s2) => s2
    | (Next(_, _) as s1, Empty) => s1
    | (Next(v1, s1), Next(v2, s2)) => {
        let order = cmp(v1, v2)
        if order <= 0 {
          Next(v1, sortedMerge(s1, append(v2->singleton, s2), cmp))
        } else {
          Next(v2, sortedMerge(append(v1->singleton, s1), s2, cmp))
        }
      }
    | (Empty, Empty) => Empty
    }
}
// skip last? put it before everything but the first

let intersperse = (seq, separator) =>
  seq
  ->mapi((~value, ~index) => index == 0 ? singleton(value) : singleton(value)->startWith(separator))
  ->flatten

module UncurriedDeferred = {
  type t<'a> = (. unit) => 'a

  type toLazy<'a> = t<'a> => Lazy.t<'a>
  let toLazy: toLazy<'a> = (f: t<'a>) => {
    let g = () => f(.)
    Lazy.from_fun(g)
  }

  type fromLazy<'a> = Lazy.t<'a> => t<'a>
  let fromLazy: fromLazy<'a> = f => (. ()) => Lazy.force(f)

  type memoize<'a> = t<'a> => t<'a>
  let memoize: memoize<'a> = f => f->toLazy->fromLazy
}

let rec cache = seq =>
  UncurriedDeferred.memoize((. ()) =>
    switch seq(.) {
    | Empty => Empty
    | Next(value, seq) => Next(value, cache(seq))
    }
  )

let allPairs = (xx: t<'a>, yy: t<'b>) => xx->flatMap(x => yy->map(y => (x, y)))

// let positioned = seq => empty // t<(seq, Head)>
// partition, split at index? consume until...take until and rest...

// like head and tail? extract?
// could someone write this themselves, and how? EOF
let consumeN = (seq, n) => {
  if n <= 0 {
    Js.Exn.raiseRangeError("Can not consume a 0 or negative amount of items.")
  }
  let consumed = []
  let isEmpty = ref(false)
  let seq = ref(seq)
  while consumed->Js.Array2.length < n && !isEmpty.contents {
    switch seq.contents(.) {
    | Empty => isEmpty := true
    | Next(head, tail) => {
        consumed->Js.Array2.push(head)->ignore
        seq := tail
      }
    }
  }
  {"consumed": consumed, "isEmpty": isEmpty.contents, "tail": seq.contents}
}

let rec chunkBySize = (seq, length) => {
  if length <= 0 {
    ArgumentOfOfRange(
      `chunkBySize requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  (. ()) => {
    let n = consumeN(seq, length)
    switch n["isEmpty"] {
    | true =>
      switch n["consumed"] {
      | [] => Empty
      | xs => Next(xs, chunkBySize(empty, length))
      }
    | false => Next(n["consumed"], chunkBySize(n["tail"], length))
    }
  }
}

// returns internal data structure
let windowed = (seq, length) => {
  if length <= 0 {
    ArgumentOfOfRange(
      `windowed requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  seq
  ->scani(~zero=[], (~sum, ~value, ~index as _) => {
    if Js.Array2.length(sum) >= length {
      sum->Js.Array2.shift->ignore
    }
    sum->Js.Array2.push(value)->ignore
    sum
  })
  ->filter(i => Js.Array2.length(i) == length)
}

let pairwise = seq =>
  seq->windowed(2)->map(i => (i->Js.Array2.unsafe_get(0), i->Js.Array2.unsafe_get(1)))

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

let toString = seq => seq->reduce("", (total, i) => total ++ i)

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
  while found.contents->Option.isNone && curr.contents !== Empty {
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

let compare = (s1, s2, cmp) =>
  zipLongest(s1, s2)
  ->map(((a, b)) =>
    switch (a, b) {
    | (Some(v1), Some(v2)) => cmp(v1, v2)
    | (None, Some(_)) => -1
    | (Some(_), None) => 1
    | (None, None) => 0
    }
  )
  ->find(i => i !== 0)
  ->Option.getWithDefault(0)

let length = seq => seq->reduce(0, (sum, _) => sum + 1)

let isEmpty = seq =>
  switch seq(.) {
  | Empty => true
  | _ => false
  }

let head = seq =>
  switch seq(.) {
  | Empty => None
  | Next(head, _) => Some(head)
  }

let headTail = seq => seq(.)->toOption

let tail = seq => seq->drop(1)

let minBy = (seq, compare) =>
  seq->reduce(None, (sum, i) => {
    switch sum {
    | None => Some(i)
    | Some(sum) => Some(compare(i, sum) < 0 ? i : sum)
    }
  })

let maxBy = (seq, compare) =>
  seq->reduce(None, (sum, i) => {
    switch sum {
    | None => Some(i)
    | Some(sum) => Some(compare(i, sum) > 0 ? i : sum)
    }
  })

let last = seq => {
  let current = ref(seq)
  let last = ref(None)
  let break = ref(false)
  while !break.contents {
    switch current.contents(.) {
    | Empty => break := true
    | Next(head, tail) => {
        last := Some(head)
        current := tail
      }
    }
  }
  last.contents
}
