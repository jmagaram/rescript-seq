module Option = Belt.Option
module Result = Belt.Result
module Ex = Extras
module TR = Extras__Trampoline

exception ArgumentOfOfRange(string)

type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | End
  | Next('a, t<'a>)

module Node = {
  let end = End

  @inline
  let next = (x, xs) => Next(x, xs)

  @inline
  let toOption = n =>
    switch n {
    | End => None
    | Next((x, xs)) => Some(x, xs)
    }

  @inline let head = n => n->toOption->Option.map(((x, _)) => x)

  @inline
  let mapNext = (n, f) =>
    switch n {
    | End => End
    | Next(x, xs) => f(x, xs)
    }
}

let empty = (. ()) => Node.end

let next = (xs: t<'a>) => xs(.)

/**
This is a foundation method for many of the functions in this library. It must
not be recursive to prevent stack overflows. This consumes at least 1 item in
`xs`.
*/
let findNode = (xs, f) => {
  let found = ref(None)
  let current = ref(xs)
  let break = ref(false)
  while !break.contents {
    switch current.contents->next {
    | End => break := true
    | Next(x, xs) as node =>
      switch f(x) {
      | true =>
        found := Some(node)
        break := true
      | false => ()
      }
      current := xs
    }
  }
  found.contents->Option.getWithDefault(End)
}

let find = (xs, f) => xs->findNode(f)->Node.head

let mapNext = (xs, f) => (. ()) => xs->next->Node.mapNext(f)

let mapBoth = (xs, ~onEmpty, ~onNext) =>
  (. ()) =>
    switch xs->next {
    | End => onEmpty()
    | Next(x, xs) => onNext(x, xs)
    }

let rec concat = (xs, ys) =>
  xs->mapBoth(~onEmpty=() => ys->next, ~onNext=(x, xs) => Node.next(x, concat(xs, ys)))

let rec flatMap = (xs, f) =>
  (. ()) =>
    switch xs->next {
    | End => End
    | Next(x, xs) => concat(f(x), flatMap(xs, f))(.)
    }

let rec map = (xs, f) => xs->mapNext((x, xs) => Node.next(f(x), map(xs, f)))

let cons = (x, xs) => (. ()) => Next(x, xs)

let head = xs => xs->next->Node.head

let headTail = xs => xs->next->Node.toOption

let forEach = (xs, f) => {
  let curr = ref(xs->next)
  let break = ref(false)
  while !break.contents {
    switch curr.contents {
    | End => break := true
    | Next(x, xs) => {
        f(x)
        curr := xs->next
      }
    }
  }
}

module Indexed = {
  type t<'a> = ('a, int)
  @inline let make = (~value, ~index) => (value, index)
  @inline let value = ((value, _): t<'a>) => value
  @inline let index = ((_, index): t<'a>) => index
  @inline let indexEquals = (i, other) => i->index == other
}

let indexed = xs => {
  let rec go = (xs, index) =>
    xs->mapNext((x, xs) => Next(Indexed.make(~value=x, ~index), go(xs, index + 1)))
  go(xs, 0)
}

let singleton = x => cons(x, empty)

let rec unfold = (seed, f) =>
  (. ()) =>
    switch f(seed) {
    | None => Node.end
    | Some(x, seed) => Node.next(x, unfold(seed, f))
    }

let init = (~count, f) => unfold(0, i => i < count ? Some(f(~index=i), i + 1) : None)

let replicate = (~count, ~value) => unfold(0, i => i < count ? Some(value, i + 1) : None)

let infinite = f => unfold(0, _ => Some(f(), 0))

let iterate = (seed, f) => unfold(seed, i => Some(i, f(i)))

let prepend = (xs, ys) => concat(ys, xs)

let range = (~start, ~end) => {
  start <= end
    ? unfold(start, i => i <= end ? Some(i, i + 1) : None)
    : unfold(start, i => i >= end ? Some(i, i - 1) : None)
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
      switch ys->next {
      | End => go(xs)(.)
      | Next(y, ys) => Next(y, go(ys))
      }
  go(xs)
}

let cycle = xs => xs->mapNext((x, xs') => cons(x, xs')->concat(xs->cycleNonEmpty)->next)

let fromString = s =>
  switch s->Js.String2.length {
  | 0 => empty
  | len => range(~start=0, ~end=len - 1)->map(inx => s->Js.String2.charAt(inx))
  }

let fromArray = (~start=?, ~end=?, xs: array<'a>) => {
  switch xs->Ex.Array.isEmpty {
  | true =>
    start
    ->Option.orElse(end)
    ->Option.forEach(_ =>
      ArgumentOfOfRange("The array is empty but you provided start and/or end indexes.")->raise
    )
    empty
  | false => {
      let len = xs->Js.Array2.length
      let start = start->Option.getWithDefault(0)
      let end = end->Option.getWithDefault(len - 1)
      if start < 0 || start > len - 1 {
        ArgumentOfOfRange(
          `The start index ${start->Belt.Int.toString} is outside the array bounds.`,
        )->raise
      }
      if end < 0 || end > len - 1 {
        ArgumentOfOfRange(
          `The end index ${start->Belt.Int.toString} is outside the array bounds.`,
        )->raise
      }
      range(~start, ~end)->map(inx => xs->Js.Array2.unsafe_get(inx))
    }
  }
}

let rec fromList = xs => {
  (. ()) =>
    switch xs {
    | list{} => End
    | list{x, ...xs} => Next(x, fromList(xs))
    }
}

let fromOption = opt =>
  switch opt {
  | None => empty
  | Some(x) => singleton(x)
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

let headTails = xs =>
  unfold(xs, xs => xs->headTail->Option.flatMap(((_, xs) as ht) => Some(ht, xs)))

let snd = ((_, b)) => b

let drop = (xs: t<'a>, count) =>
  switch count {
  | 0 => xs
  | n if n < 0 =>
    ArgumentOfOfRange(
      `'drop' requires a count of zero or more but youu asked for ${count->Belt.Int.toString}`,
    )->raise
  | count =>
    xs
    ->headTails
    ->indexed
    ->find(Indexed.indexEquals(_, count - 1))
    ->Option.map(Indexed.value)
    ->Option.map(snd)
    ->Option.getWithDefault(empty)
  }

let rec filter = (xs, f) =>
  (. ()) => {
    switch xs->next {
    | End => End
    | Next(x, xs) =>
      switch f(x) {
      | true => Next(x, filter(xs, f))
      | false =>
        switch xs->headTails->find(((x, _)) => f(x)) {
        | None => End
        | Some((x, xs)) => Next(x, filter(xs, f))
        }
      }
    }
  }

let filteri = (xs, f) =>
  xs->indexed->filter(((value, index)) => f(~value, ~index))->map(((v, _)) => v)

let rec zipLongest = (xs, ys) => {
  (. ()) => {
    let xn = xs->next
    let yn = ys->next
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

let rec zip = (xs, ys) =>
  (. ()) => {
    switch (xs->next, ys->next) {
    | (End, _) => End
    | (_, End) => End
    | (Next(x, xs), Next(y, ys)) => Next((x, y), zip(xs, ys))
    }
  }

let filterMap = (xs, f) => xs->map(f)->filter(Option.isSome)->map(Option.getUnsafe)

let filterSome = xs => xs->filterMap(x => x)

let filterOk = xs =>
  xs->filterMap(x =>
    switch x {
    | Ok(ok) => Some(ok)
    | Error(_) => None
    }
  )

// need stress millions test on this
let scani = (xs, ~zero, f) => {
  let rec go = (seq, sum) =>
    switch seq->next {
    | End => (. ()) => End
    | Next((value, index), seq) =>
      (. ()) => {
        let sum = f(~sum, ~value, ~index)
        Next(sum, go(seq, sum))
      }
    }
  concat(singleton(zero), go(xs->indexed, zero))
}

let scan = (seq, zero, f) => scani(seq, ~zero, (~sum, ~value, ~index as _) => f(sum, value))

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

let intersperse = (xs, separator) =>
  xs
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
    switch seq->next {
    | End => End
    | Next(value, seq) => Next(value, cache(seq))
    }
  )

let allPairs = (xx: t<'a>, yy: t<'b>) => xx->flatMap(x => yy->map(y => (x, y)))

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
    switch seq.contents->next {
    | End => isEmpty := true
    | Next(head, tail) => {
        consumed->Js.Array2.push(head)->ignore
        seq := tail
      }
    }
  }
  {"consumed": consumed, "isEmpty": isEmpty.contents, "tail": seq.contents}
}

let dropUntil = (xs, predicate) =>
  (. ()) =>
    xs
    ->headTails
    ->find(((x, _)) => predicate(x))
    ->Option.map(((x, xs)) => Node.next(x, xs))
    ->Option.getWithDefault(Node.end)

let dropWhile = (xs, predicate) =>
  (. ()) =>
    xs
    ->headTails
    ->find(((x, _)) => false == predicate(x))
    ->Option.map(((x, xs)) => Next(x, xs))
    ->Option.getWithDefault(Node.end)

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

let pairwise = xs =>
  xs->window(2)->map(i => (i->Js.Array2.unsafe_get(0), i->Js.Array2.unsafe_get(1)))

let reduce = (xs, zero, concat) => {
  let sum = ref(zero)
  xs->forEach(x => sum := concat(sum.contents, x))
  sum.contents
}

let reducei = (seq, zero, concat) =>
  seq->indexed->reduce(zero, (sum, (value, index)) => concat(~sum, ~value, ~index))

let last = xs => xs->reduce(None, (_, x) => Some(x))

let toArray = seq =>
  seq->reduce([], (arr, i) => {
    arr->Js.Array2.push(i)->ignore
    arr
  })

let toString = seq => seq->reduce("", (total, i) => total ++ i)

let forEachi = (seq, f) => seq->indexed->forEach(((value, index)) => f(~value, ~index))

let some = (seq, predicate) => {
  let break = ref(false)
  let curr = ref(seq->next)
  let result = ref(false)
  while result.contents !== true && curr.contents !== End {
    switch curr.contents {
    | End => break := true
    | Next(value, seq) => {
        result := predicate(value)
        curr := seq->next
      }
    }
  }
  result.contents
}

let everyOrEmpty = (seq, predicate) => {
  let break = ref(false)
  let curr = ref(seq->next)
  let foundInvalid = ref(false)
  while foundInvalid.contents === false && curr.contents !== End {
    switch curr.contents {
    | End => break := true
    | Next(value, seq) => {
        foundInvalid := !predicate(value)
        curr := seq->next
      }
    }
  }
  !foundInvalid.contents
}

let findMapi = (seq, f) => {
  let seq = seq->indexed
  let curr = ref(seq->next)
  let found = ref(None)
  while found.contents->Option.isNone && curr.contents !== End {
    switch curr.contents {
    | End => ()
    | Next((value, index), seq) => {
        switch f(~value, ~index) {
        | None => ()
        | Some(_) as m => found := m
        }
        curr := seq->next
      }
    }
  }
  found.contents
}

let findMap = (seq, f) => findMapi(seq, (~value, ~index as _) => f(value))

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
  switch seq->next {
  | End => true
  | _ => false
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

let rec interleave = (xs, ys) => {
  (. ()) => {
    switch xs->next {
    | End => ys->next
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
  } else {
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
}

let windowAhead = (xs, size) => {
  if size <= 0 {
    ArgumentOfOfRange(`windowAhead requires a size greater than zero.`)->raise
  } else {
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
}

let rec takeUntil = (xs, f) =>
  xs->mapNext((x, xs) => {
    switch f(x) {
    | false => Next(x, takeUntil(xs, f))
    | true => Next(x, empty)
    }
  })

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
  switch seq->next {
  | End => None
  | Next(head, tail) => Some(tail->startWith(head))
  }
