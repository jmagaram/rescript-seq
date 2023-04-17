module Option = Belt.Option
module Result = Belt.Result
module Ex = Extras

type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | End
  | Next('a, t<'a>)

exception ArgumentOfOfRange(string)

let consume1 = xs => xs(.)

let consumeUntil = (~seq, ~predicate, ~onNext, ~onEmpty) => {
  let break = ref(false)
  let seq = ref(seq)
  while !break.contents {
    switch seq.contents(.) {
    | End =>
      break := true
      onEmpty()
    | Next(head, tail) =>
      break := predicate(head)
      seq := tail
      onNext(head, tail)
    }
  }
}

let mapNext = (xs, f) =>
  (. ()) =>
    switch xs(.) {
    | End => End
    | Next(x, xs) => f(x, xs)
    }

let mapBoth = (xs, ~onEmpty, ~onNext) =>
  (. ()) =>
    switch xs(.) {
    | End => onEmpty()
    | Next(x, xs) => onNext(x, xs)
    }

let empty = (. ()) => End

let cons = (value, seq) => (. ()) => Next(value, seq)

let singleton = v => cons(v, empty)

let rec unfold = (seed, f) =>
  (. ()) => {
    switch f(seed) {
    | None => End
    | Some(value, seed) => Next(value, unfold(seed, f))
    }
  }

let init = (~count, f) => unfold(0, i => i < count ? Some(f(~index=i), i + 1) : None)

let replicate = (~count, ~value) => unfold(0, i => i < count ? Some(value, i + 1) : None)

let infinite = f => unfold(0, _ => Some(f(), 0))

let iterate = (seed, f) => unfold(seed, i => Some(i, f(i)))

let rec concat = (xs, ys) =>
  xs->mapBoth(~onEmpty=() => ys->consume1, ~onNext=(x, xs) => Next(x, concat(xs, ys)))

let prepend = (xs, ys) => concat(ys, xs)

let range = (~start, ~end) => {
  start <= end
    ? unfold(start, i => i <= end ? Some(i, i + 1) : None)
    : unfold(start, i => i >= end ? Some(i, i - 1) : None)
}

let rec flatMap = (xs, f) => {
  (. ()) =>
    switch xs(.) {
    | End => End
    | Next(value, next) => concat(f(value), flatMap(next, f))(.)
    }
}

let rec tap = (xs, f) =>
  xs->mapNext((x, xs) => {
    f(x)
    Next(x, tap(xs, f))
  })

let startWith = (xs, x) => cons(x, xs)

let flatten = xxs => xxs->flatMap(i => i)

let cycleNonEmpty = xs => {
  let rec go = ys =>
    (. ()) =>
      switch ys(.) {
      | End => go(xs)(.)
      | Next(y, ys) => Next(y, go(ys))
      }
  go(xs)
}

let cycle = xs => xs->mapNext((x, xs') => cons(x, xs')->concat(xs->cycleNonEmpty)->consume1)

let rec map = (xs, f) => xs->mapNext((x, xs) => Next(f(x), map(xs, f)))

let fromString = s =>
  switch s->Js.String2.length {
  | 0 => empty
  | len => range(~start=0, ~end=len - 1)->map(inx => s->Js.String2.charAt(inx))
  }

let fromArray = (~start=?, ~end=?, arr: array<'a>) => {
  switch arr->Ex.Array.isEmpty {
  | true => empty
  | false => {
      let start = start->Option.getWithDefault(0)
      let end = end->Option.getWithDefault(arr->Ex.Array.lastIndex->Option.getUnsafe)
      range(~start, ~end)->map(inx => arr->Js.Array2.unsafe_get(inx))
    }
  }
}

let rec fromList = xs => {
  (. ()) =>
    switch xs {
    | list{} => End
    | list{head, ...tail} => Next(head, fromList(tail))
    }
}

let fromOption = opt =>
  switch opt {
  | None => empty
  | Some(value) => singleton(value)
  }

let indexed = xs => {
  let rec go = (xs, inx) => xs->mapNext((x, xs) => Next((x, inx), go(xs, inx + 1)))
  go(xs, 0)
}

let mapi = (xs, f) => xs->indexed->map(((x, index)) => f(~value=x, ~index))

let takeAtMost = (xs, count) => {
  let rec go = xs =>
    xs->mapNext(((x, index), xs) =>
      switch index >= count {
      | true => End
      | false => Next(x, go(xs))
      }
    )
  go(xs->indexed)
}

let drop = (xs, count) => {
  let rec go = xs =>
    xs->mapNext(((x, index), xs) =>
      switch index < count {
      | true => go(xs)->consume1
      | false => Next(x, xs->map(((x, _)) => x))
      }
    )
  go(xs->indexed)
}

let filteri = (xs, f) => {
  let rec go = xs =>
    xs->mapNext(((x, index), xs) =>
      switch f(~value=x, ~index) {
      | true => Next(x, go(xs))
      | false => go(xs)->consume1
      }
    )
  go(xs->indexed)
}

let rec filter = (xs, f) =>
  xs->mapNext((x, xs) =>
    switch f(x) {
    | true => Next(x, filter(xs, f))
    | false => filter(xs, f)->consume1
    }
  )

let rec zipLongest = (xs, ys) => {
  (. ()) => {
    let xn = xs->consume1
    let yn = ys->consume1
    switch (xn, yn) {
    | (End, End) => End
    | (Next(x, xs), End) => Next((Some(x), None), zipLongest(xs, empty))
    | (End, Next(y, ys)) => Next((None, Some(y)), zipLongest(empty, ys))
    | (Next(x, xs), Next(y, ys)) => Next((Some(x), Some(y)), zipLongest(xs, ys))
    }
  }
}

let rec takeWhile = (xs, predicate) =>
  xs->mapNext((x, xs) =>
    switch predicate(x) {
    | false => End
    | true => Next(x, takeWhile(xs, predicate))
    }
  )

// let rec filterMap2 = (xs, f) =>
//   xs->mapNext((x, xs) => {
//     let c = xs->map(f)->takeUntil(Option.isSome)
//   })

let rec zip = (xs, ys) =>
  (. ()) => {
    switch (xs->consume1, ys->consume1) {
    | (End, _) => End
    | (_, End) => End
    | (Next(x, xs), Next(y, ys)) => Next((x, y), zip(xs, ys))
    }
  }

let rec filterMap = (xs, f) =>
  xs->mapNext((x, xs) =>
    switch f(x) {
    | Some(x) => Next(x, filterMap(xs, f))
    | None => {
        let xs' = ref(None)
        let x = ref(None)
        consumeUntil(
          ~seq=xs,
          ~onEmpty=() => (),
          ~onNext=(_, xs) => xs' := Some(xs),
          ~predicate=i => {
            x := f(i)
            x.contents->Option.isSome
          },
        )
        switch x.contents {
        | None => End
        | Some(x) => Next(x, filterMap(xs'.contents->Option.getExn, f))
        }
      }
    }
  )

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
    | End => (. ()) => End
    | Next((value, index), seq) =>
      (. ()) => {
        let sum = f(~sum, ~value, ~index)
        Next(sum, go(seq, sum))
      }
    }
  concat(singleton(zero), go(seq->indexed, zero))
}

let scan = (seq, zero, f) => scani(seq, ~zero, (~sum, ~value, ~index as _) => f(sum, value))

let rec dropWhile = (seq, predicate) => {
  (. ()) => {
    switch seq(.) {
    | End => End
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
    | (End, Next(_, _) as s2) => s2
    | (Next(_, _) as s1, End) => s1
    | (Next(v1, s1), Next(v2, s2)) => {
        let order = cmp(v1, v2)
        if order <= 0 {
          Next(v1, sortedMerge(s1, concat(v2->singleton, s2), cmp))
        } else {
          Next(v2, sortedMerge(concat(v1->singleton, s1), s2, cmp))
        }
      }
    | (End, End) => End
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
    | End => End
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
    | End => isEmpty := true
    | Next(head, tail) => {
        consumed->Js.Array2.push(head)->ignore
        seq := tail
      }
    }
  }
  {"consumed": consumed, "isEmpty": isEmpty.contents, "tail": seq.contents}
}

let dropUntil = (seq, predicate) =>
  (. ()) => {
    let start = ref(None)
    consumeUntil(
      ~seq,
      ~predicate,
      ~onNext=(head, tail) => {start := Some((head, tail))},
      ~onEmpty=() => start := None,
    )
    switch start.contents {
    | None => End
    | Some(head, tail) => Next(head, tail)
    }
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
      | [] => End
      | xs => Next(xs, chunkBySize(empty, length))
      }
    | false => Next(n["consumed"], chunkBySize(n["tail"], length))
    }
  }
}

// returns internal data structure
let window = (seq, length) => {
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
  seq->window(2)->map(i => (i->Js.Array2.unsafe_get(0), i->Js.Array2.unsafe_get(1)))

let reduce = (seq, zero, concat) => {
  let sum = ref(zero)
  let curr = ref(seq(.))
  while curr.contents !== End {
    switch curr.contents {
    | End => ()
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
  while curr.contents !== End {
    switch curr.contents {
    | End => ()
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
  while result.contents !== true && curr.contents !== End {
    switch curr.contents {
    | End => break := true
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
  while foundInvalid.contents === false && curr.contents !== End {
    switch curr.contents {
    | End => break := true
    | Next(value, seq) => {
        foundInvalid := !predicate(value)
        curr := seq(.)
      }
    }
  }
  !foundInvalid.contents
}

let findMapi = (seq, f) => {
  let seq = seq->indexed
  let curr = ref(seq(.))
  let found = ref(None)
  while found.contents->Option.isNone && curr.contents !== End {
    switch curr.contents {
    | End => ()
    | Next((value, index), seq) => {
        switch f(~value, ~index) {
        | None => ()
        | Some(_) as m => found := m
        }
        curr := seq(.)
      }
    }
  }
  found.contents
}

let findMap = (seq, f) => findMapi(seq, (~value, ~index as _) => f(value))

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
  | End => true
  | _ => false
  }

let head = seq =>
  switch seq(.) {
  | End => None
  | Next(head, _) => Some(head)
  }

let headTail = seq =>
  switch seq(.) {
  | End => None
  | Next(head, tail) => Some(head, tail)
  }

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
    | End => break := true
    | Next(head, tail) => {
        last := Some(head)
        current := tail
      }
    }
  }
  last.contents
}

let rec interleave = (xs, ys) => {
  (. ()) => {
    switch xs(.) {
    | End => ys(.)
    | Next(x, xs) => Next(x, interleave(ys, xs))
    }
  }
}

let interleaveMany = xxs => {
  switch xxs->Js.Array2.length {
  | 0 => empty
  | length => {
      let xxs = xxs->Js.Array2.map(i => Some(i))
      let remain = ref(length)
      let consumeHead = inx => {
        xxs
        ->Js.Array2.unsafe_get(inx)
        ->Option.flatMap(xs => {
          switch xs->headTail {
          | None =>
            remain := remain.contents - 1
            xxs->Js.Array2.unsafe_set(inx, None)
            None
          | Some(h, t) =>
            xxs->Js.Array2.unsafe_set(inx, Some(t))
            Some(h)
          }
        })
      }
      range(~start=0, ~end=length - 1)
      ->cycle
      ->map(consumeHead)
      ->takeWhile(_ => remain.contents > 0)
      ->filterSome
    }
  }
}

let toExactlyOne = xs =>
  switch xs->headTail {
  | None => None
  | Some(head, tail) =>
    switch tail->isEmpty {
    | true => Some(head)
    | false => None
    }
  }

let isSortedBy = (xs, cmp) => xs->pairwise->everyOrEmpty(((a, b)) => cmp(a, b) <= 0)

let windowBehind = (xs, size) => {
  if size <= 0 {
    ArgumentOfOfRange(`windowBehind requires a size greater than zero.`)->raise
  }
  xs
  ->scan([], (sum, i) => {
    if sum->Js.Array2.length === size {
      sum->Js.Array2.shift->ignore
    }
    sum->Js.Array2.push(i)->ignore
    sum
  })
  ->drop(1)
}

let windowAhead = (xs, size) => {
  if size <= 0 {
    ArgumentOfOfRange(`windowAhead requires a size greater than zero.`)->raise
  }
  xs
  ->map(i => Some(i))
  ->concat(replicate(~count=size - 1, ~value=None))
  ->scani(~zero=[], (~sum, ~value as i, ~index) => {
    if index >= size {
      sum->Js.Array2.shift->ignore
    }
    i->Option.forEach(i => sum->Js.Array2.push(i)->ignore)
    sum
  })
  ->drop(size)
}

let rec takeUntil = (xs, f) => xs->mapNext((x, xs) => Next(x, f(x) ? empty : takeUntil(xs, f)))

let allOk = seq => {
  seq
  ->scan(Ok(empty), (sum, i) =>
    switch i {
    | Ok(ok) => sum->Result.map(oks => concat(oks, singleton(ok)))
    | Error(_) as err => err
    }
  )
  ->takeUntil(Result.isError(_))
  ->last
  ->Option.getUnsafe
}

let allSome = seq => {
  seq
  ->scan(Some(empty), (sum, i) =>
    switch i {
    | Some(ok) => sum->Option.map(oks => concat(oks, singleton(ok)))
    | None => None
    }
  )
  ->takeUntil(Option.isNone(_))
  ->last
  ->Option.flatMap(i => i)
}

let toOption = seq =>
  switch seq(.) {
  | End => None
  | Next(head, tail) => Some(tail->startWith(head))
  }
