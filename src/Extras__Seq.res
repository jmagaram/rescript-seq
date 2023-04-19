module Option = Belt.Option
module Result = Belt.Result
module Ex = Extras

exception ArgumentOfOfRange(string)

type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | End
  | Next('a, t<'a>)

module Node = {
  let end = End

  @inline
  let next = (x, xx) => Next(x, xx)

  @inline
  let toOption = n =>
    switch n {
    | End => None
    | Next((x, xx)) => Some(x, xx)
    }

  @inline let head = n => n->toOption->Option.map(((x, _)) => x)

  @inline
  let mapNext = (n, f) =>
    switch n {
    | End => End
    | Next(x, xx) => f(x, xx)
    }
}

let empty = (. ()) => Node.end

let nextNode = (xx: t<'a>) => xx(.)
let next = xx => xx->nextNode->Node.toOption

let cons = (x, xx) => (. ()) => Next(x, xx)
let startWith = (xx, x) => cons(x, xx)

let singleton = x => cons(x, empty)

/**
This is a foundation method for many of the functions in this library. It must
not be recursive to prevent stack overflows. This consumes at least 1 item in
`xx`.
*/
let findNode = (xx, f) => {
  let found = ref(None)
  let current = ref(xx)
  let break = ref(false)
  while !break.contents {
    switch current.contents->nextNode {
    | End => break := true
    | Next(x, xx) as node =>
      switch f(x) {
      | true =>
        found := Some(node)
        break := true
      | false => ()
      }
      current := xx
    }
  }
  found.contents->Option.getWithDefault(End)
}

let find = (xx, f) => xx->findNode(f)->Node.head

let mapNext = (xx, f) => (. ()) => xx->nextNode->Node.mapNext(f)

let rec concat = (xx, yy) =>
  (. ()) => {
    switch xx->nextNode {
    | End => yy->nextNode
    | Next(x, xx) => Next(x, concat(xx, yy))
    }
  }

let endWith = (xx, x) => concat(xx, singleton(x))
let prepend = (xx, yy) => concat(yy, xx)

let rec flatMap = (xx, f) =>
  (. ()) =>
    switch xx->nextNode {
    | End => End
    | Next(x, xx) => concat(f(x), flatMap(xx, f))(.)
    }

let flatten = xxx => xxx->flatMap(i => i)

let rec map = (xx, f) =>
  (. ()) => {
    switch xx->nextNode {
    | End => End
    | Next(x, xx) => Next(f(x), map(xx, f))
    }
  }

let head = xx =>
  switch xx->nextNode {
  | End => None
  | Next(x, _) => Some(x)
  }

let headTail = xx =>
  switch xx->nextNode {
  | End => None
  | Next(xx, x) => Some(xx, x)
  }

let forEach = (xx, f) => {
  let curr = ref(xx->nextNode)
  let break = ref(false)
  while !break.contents {
    switch curr.contents {
    | End => break := true
    | Next(x, xx) => {
        f(x)
        curr := xx->nextNode
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

let indexed = xx => {
  let rec go = (xx, index) =>
    (. ()) =>
      switch xx->nextNode {
      | End => End
      | Next(x, xx) => Next(Indexed.make(~value=x, ~index), go(xx, index + 1))
      }
  go(xx, 0)
}

let rec unfold = (seed, f) =>
  (. ()) =>
    switch f(seed) {
    | None => End
    | Some(x, seed) => Next(x, unfold(seed, f))
    }

let init = (~count, f) => unfold(0, i => i < count ? Some(f(~index=i), i + 1) : None)

let replicate = (~count, ~value) => unfold(0, i => i < count ? Some(value, i + 1) : None)

let iterate = (seed, f) => unfold(seed, i => Some(i, f(i)))

let range = (~start, ~end) => {
  start <= end
    ? unfold(start, i => i <= end ? Some(i, i + 1) : None)
    : unfold(start, i => i >= end ? Some(i, i - 1) : None)
}

let rec infinite = f => (. ()) => Next(f(), infinite(f))

let rec tap = (xx, f) =>
  (. ()) =>
    switch xx->nextNode {
    | End => End
    | Next(x, xx) => {
        f(x)
        Next(x, tap(xx, f))
      }
    }

let cycleNonEmpty = xx => {
  let rec go = yy =>
    (. ()) =>
      switch yy->nextNode {
      | End => go(xx)(.)
      | Next(y, yy) => Next(y, go(yy))
      }
  go(xx)
}

let cycle = xx => xx->mapNext((x, xx') => cons(x, xx')->concat(xx->cycleNonEmpty)->nextNode)

let fromString = s =>
  switch s->Js.String2.length {
  | 0 => empty
  | len => range(~start=0, ~end=len - 1)->map(inx => s->Js.String2.charAt(inx))
  }

let fromArray = (~start=?, ~end=?, xx: array<'a>) => {
  switch xx->Ex.Array.isEmpty {
  | true =>
    start
    ->Option.orElse(end)
    ->Option.forEach(_ =>
      ArgumentOfOfRange("The array is empty but you provided start and/or end indexes.")->raise
    )
    empty
  | false => {
      let len = xx->Js.Array2.length
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
      range(~start, ~end)->map(inx => xx->Js.Array2.unsafe_get(inx))
    }
  }
}

let rec fromList = xx => {
  (. ()) =>
    switch xx {
    | list{} => End
    | list{x, ...xx} => Next(x, fromList(xx))
    }
}

let fromOption = opt =>
  switch opt {
  | None => empty
  | Some(x) => singleton(x)
  }

let mapi = (xx, f) => xx->indexed->map(((x, index)) => f(~value=x, ~index))

let takeAtMost = (xx, count) => {
  if count == 0 {
    empty
  } else {
    let rec go = xx =>
      xx->mapNext(((x, index), xx) =>
        switch index >= count {
        | true => End
        | false => Next(x, go(xx))
        }
      )
    go(xx->indexed)
  }
}

let headTails = xx =>
  unfold(xx, xx => xx->headTail->Option.flatMap(((_, xx) as ht) => Some(ht, xx)))

let snd = ((_, b)) => b

let drop = (xx, count) =>
  switch count {
  | 0 => xx
  | n if n < 0 =>
    ArgumentOfOfRange(
      `'drop' requires a count of zero or more but youu asked for ${count->Belt.Int.toString}`,
    )->raise
  | count =>
    xx
    ->headTails
    ->indexed
    ->find(Indexed.indexEquals(_, count - 1))
    ->Option.map(Indexed.value)
    ->Option.map(snd)
    ->Option.getWithDefault(empty)
  }

let rec filter = (xx, f) =>
  (. ()) => {
    switch xx->nextNode {
    | End => End
    | Next(x, xx) =>
      switch f(x) {
      | true => Next(x, filter(xx, f))
      | false =>
        switch xx->headTails->find(((x, _)) => f(x)) {
        | None => End
        | Some((x, xx)) => Next(x, filter(xx, f))
        }
      }
    }
  }

let filteri = (xx, f) =>
  xx->indexed->filter(((value, index)) => f(~value, ~index))->map(((v, _)) => v)

let rec takeWhile = (xx, predicate) =>
  xx->mapNext((x, xx) =>
    switch predicate(x) {
    | false => End
    | true => Next(x, takeWhile(xx, predicate))
    }
  )

let rec takeUntil = (xx, f) =>
  xx->mapNext((x, xx) => {
    switch f(x) {
    | false => Next(x, takeUntil(xx, f))
    | true => Next(x, empty)
    }
  })

let filterMap = (xx, f) => xx->map(f)->filter(Option.isSome)->map(Option.getUnsafe)

let filterSome = xx => xx->filterMap(x => x)

let filterOk = xx =>
  xx->filterMap(x =>
    switch x {
    | Ok(ok) => Some(ok)
    | Error(_) => None
    }
  )

let scani = (xx, ~zero, f) => {
  let rec go = (xx, sum) =>
    (. ()) =>
      switch xx->nextNode {
      | End => End
      | Next((x, index), xx) => {
          let sum = f(~sum, ~value=x, ~index)
          Next(sum, go(xx, sum))
        }
      }
  concat(singleton(zero), go(xx->indexed, zero))
}

let scan = (xx, zero, f) => scani(xx, ~zero, (~sum, ~value, ~index as _) => f(sum, value))

let rec sortedMerge = (xx, yy, cmp) => {
  (. ()) =>
    switch (xx(.), yy(.)) {
    | (End, Next(_, _) as yy) => yy
    | (Next(_, _) as xx, End) => xx
    | (Next(x, xx), Next(y, yy)) => {
        let order = cmp(x, y)
        if order <= 0 {
          Next(x, sortedMerge(xx, concat(y->singleton, yy), cmp))
        } else {
          Next(y, sortedMerge(concat(x->singleton, xx), yy, cmp))
        }
      }
    | (End, End) => End
    }
}

let intersperse = (xx, separator) =>
  xx
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
    switch seq->nextNode {
    | End => End
    | Next(value, seq) => Next(value, cache(seq))
    }
  )

let allPairs = (xx, yy) => xx->flatMap(x => yy->map(y => (x, y)))

let dropUntil = (xx, predicate) =>
  (. ()) =>
    xx
    ->headTails
    ->find(((x, _)) => predicate(x))
    ->Option.map(((x, xx)) => Node.next(x, xx))
    ->Option.getWithDefault(Node.end)

let dropWhile = (xx, predicate) =>
  (. ()) =>
    xx
    ->headTails
    ->find(((x, _)) => false == predicate(x))
    ->Option.map(((x, xx)) => Next(x, xx))
    ->Option.getWithDefault(Node.end)

let chunkBySize = (xx, length) => {
  if length <= 0 {
    ArgumentOfOfRange(
      `chunkByyize requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  xx
  ->map(i => Some(i))
  ->concat(replicate(~count=length - 1, ~value=None))
  ->scani(~zero=[], (~sum, ~value, ~index) => {
    switch value {
    | None => sum
    | Some(value) =>
      switch mod(index, length) {
      | 0 => [value]
      | _ =>
        sum->Js.Array2.push(value)->ignore
        sum
      }
    }
  })
  ->filteri((~value as _, ~index) => mod(index, length) == 0)
  ->drop(1)
}

let window = (xx, length) => {
  if length <= 0 {
    ArgumentOfOfRange(
      `windowed requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  xx
  ->scani(~zero=[], (~sum, ~value, ~index as _) => {
    if Js.Array2.length(sum) >= length {
      sum->Js.Array2.shift->ignore
    }
    sum->Js.Array2.push(value)->ignore
    sum
  })
  ->filter(i => Js.Array2.length(i) == length)
}

let pairwise = xx =>
  xx->window(2)->map(i => (i->Js.Array2.unsafe_get(0), i->Js.Array2.unsafe_get(1)))

let reduce = (xx, zero, concat) => {
  let sum = ref(zero)
  xx->forEach(x => sum := concat(sum.contents, x))
  sum.contents
}

let reducei = (xx, zero, concat) =>
  xx->indexed->reduce(zero, (sum, (value, index)) => concat(~sum, ~value, ~index))

let last = xx => xx->reduce(None, (_, x) => Some(x))

let toArray = xx =>
  xx->reduce([], (xx, i) => {
    xx->Js.Array2.push(i)->ignore
    xx
  })

let toString = xx => xx->reduce("", (total, i) => total ++ i)

let forEachi = (xx, f) => xx->indexed->forEach(((value, index)) => f(~value, ~index))

let some = (xx, f) => xx->find(f)->Option.isSome

let everyOrEmpty = (xx, f) => xx->find(i => !f(i))->Option.isNone

let findMapi = (xx, f) =>
  xx
  ->mapi((~value, ~index) => f(~value, ~index))
  ->find(Option.isSome(_))
  ->Option.map(Option.getUnsafe)

let findMap = (xx, f) => findMapi(xx, (~value, ~index as _) => f(value))

let rec map2 = (xx, yy, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    Ex.Option.map2(xx, yy, ((x, xx), (y, yy)) => Next(
      f(x, y),
      map2(xx, yy, f),
    ))->Option.getWithDefault(End)
  }

let rec map3 = (xx, yy, zz, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    let zz = zz->next
    Ex.Option.map3(xx, yy, zz, ((x, xx), (y, yy), (z, zz)) => Next(
      f(x, y, z),
      map3(xx, yy, zz, f),
    ))->Option.getWithDefault(End)
  }

let rec map4 = (xx, yy, zz, qq, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    let zz = zz->next
    let qq = qq->next
    Ex.Option.map4(xx, yy, zz, qq, ((x, xx), (y, yy), (z, zz), (q, qq)) => Next(
      f(x, y, z, q),
      map4(xx, yy, zz, qq, f),
    ))->Option.getWithDefault(End)
  }

let rec map5 = (xx, yy, zz, qq, mm, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    let zz = zz->next
    let qq = qq->next
    let mm = mm->next
    Ex.Option.map5(xx, yy, zz, qq, mm, ((x, xx), (y, yy), (z, zz), (q, qq), (m, mm)) => Next(
      f(x, y, z, q, m),
      map5(xx, yy, zz, qq, mm, f),
    ))->Option.getWithDefault(End)
  }

let zip = (xx, yy) => map2(xx, yy, (x, y) => (x, y))
let zip3 = (xx, yy, zz) => map3(xx, yy, zz, (x, y, z) => (x, y, z))
let zip4 = (xx, yy, zz, qq) => map4(xx, yy, zz, qq, (x, y, z, q) => (x, y, z, q))
let zip5 = (xx, yy, zz, qq, mm) => map5(xx, yy, zz, qq, mm, (x, y, z, q, m) => (x, y, z, q, m))

let equals = (xx, yy, eq) => {
  let xx = xx->map(x => Some(x))->endWith(None)
  let yy = yy->map(y => Some(y))->endWith(None)
  zip(xx, yy)->everyOrEmpty(((x, y)) =>
    switch (x, y) {
    | (Some(x), Some(y)) => eq(x, y)
    | (None, None) => true
    | _ => false
    }
  )
}

let compare = (xx, yy, cmp) => {
  let xx = xx->map(x => Some(x))->endWith(None)
  let yy = yy->map(y => Some(y))->endWith(None)
  zip(xx, yy)
  ->map(((x, y)) =>
    switch (x, y) {
    | (Some(x), Some(y)) => cmp(x, y)
    | (None, Some(_)) => -1
    | (Some(_), None) => 1
    | (None, None) => 0
    }
  )
  ->find(i => i !== 0)
  ->Option.getWithDefault(0)
}

let length = xx => xx->reduce(0, (sum, _) => sum + 1)

let isEmpty = xx =>
  switch xx->nextNode {
  | End => true
  | _ => false
  }

let tail = xx => xx->drop(1)

let minBy = (xx, cmp) =>
  xx->reduce(None, (sum, x) => {
    switch sum {
    | None => Some(x)
    | Some(sum) => Some(cmp(x, sum) < 0 ? x : sum)
    }
  })

let maxBy = (xx, cmp) =>
  xx->reduce(None, (sum, x) => {
    switch sum {
    | None => Some(x)
    | Some(sum) => Some(cmp(x, sum) > 0 ? x : sum)
    }
  })

let rec interleave = (xx, yy) => {
  (. ()) => {
    switch xx->nextNode {
    | End => yy->nextNode
    | Next(x, xx) => Next(x, interleave(yy, xx))
    }
  }
}

let interleaveMany = xxx => {
  switch xxx->Js.Array2.length {
  | 0 => empty
  | length => {
      let xxx = xxx->Js.Array2.map(i => Some(i))
      let remain = ref(length)
      let consumeHead = inx => {
        xxx
        ->Js.Array2.unsafe_get(inx)
        ->Option.flatMap(xx => {
          switch xx->headTail {
          | None =>
            remain := remain.contents - 1
            xxx->Js.Array2.unsafe_set(inx, None)
            None
          | Some(h, t) =>
            xxx->Js.Array2.unsafe_set(inx, Some(t))
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

let toExactlyOne = xx =>
  switch xx->headTail {
  | None => None
  | Some(x, xx) =>
    switch xx->isEmpty {
    | true => Some(x)
    | false => None
    }
  }

let isSortedBy = (xx, cmp) => xx->pairwise->everyOrEmpty(((a, b)) => cmp(a, b) <= 0)

// stopped here!
let windowBehind = (xx, size) => {
  if size <= 0 {
    ArgumentOfOfRange(`windowBehind requires a size greater than zero.`)->raise
  } else {
    xx
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

let windowAhead = (xx, size) => {
  if size <= 0 {
    ArgumentOfOfRange(`windowAhead requires a size greater than zero.`)->raise
  } else {
    xx
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

let allOk = xx => {
  xx
  ->scan(Ok(empty), (sum, x) =>
    switch x {
    | Ok(ok) => sum->Result.map(oks => concat(oks, singleton(ok)))
    | Error(_) as err => err
    }
  )
  ->takeUntil(Result.isError(_))
  ->last
  ->Option.getUnsafe
}

let allSome = xx => {
  xx
  ->scan(Some(empty), (sum, x) =>
    switch x {
    | Some(ok) => sum->Option.map(oks => concat(oks, singleton(ok)))
    | None => None
    }
  )
  ->takeUntil(Option.isNone(_))
  ->last
  ->Option.flatMap(i => i)
}

let toOption = xx =>
  switch xx->nextNode {
  | End => None
  | Next(x, xx) => Some(xx->startWith(x))
  }
