module Option = Belt.Option
module Result = Belt.Result
module OptionEx = Extras__Option

exception InvalidArgument(string)

type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | End
  | Next('a, t<'a>)

let delay = generator => (. ()) => generator()(.)

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

let once = x => (. ()) => Next(x, empty)

let onceWith = f => (. ()) => Next(f(), empty)

let cons = (x, xx) => (. ()) => Next(x, xx)
let startWith = (xx, x) => (. ()) => Next(x, xx)

let nextNode = (xx: t<'a>) => xx(.)
let next = xx => xx->nextNode->Node.toOption

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

let endWith = (xx, x) => concat(xx, once(x))
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

let init = (count, f) => unfold(0, i => i < count ? Some(f(i), i + 1) : None)

let repeat = (count, value) => unfold(0, i => i < count ? Some(value, i + 1) : None)

let rec forever = value => (. ()) => Next(value, forever(value))

let rec foreverWith = f => (. ()) => Next(f(), delay(() => foreverWith(f)))

let repeatWith = (count, value) => unfold(1, i => i <= count ? Some(value(), i + 1) : None)

let iterate = (seed, f) => unfold(seed, i => Some(i, f(i)))

let range = (start, end) => {
  start <= end
    ? unfold(start, i => i <= end ? Some(i, i + 1) : None)
    : unfold(start, i => i >= end ? Some(i, i - 1) : None)
}

let rangeMap = (start, end, f) => range(start, end)->map(f)

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

let fromArray = (~start=?, ~end=?, xx: array<'a>) => {
  switch xx->Extras__Array.isEmpty {
  | true =>
    start
    ->Option.orElse(end)
    ->Option.forEach(_ =>
      InvalidArgument("The array is empty but you provided start and/or end indexes.")->raise
    )
    empty
  | false => {
      let len = xx->Js.Array2.length
      let start = start->Option.getWithDefault(0)
      let end = end->Option.getWithDefault(len - 1)
      if start < 0 || start > len - 1 {
        InvalidArgument(
          `The start index ${start->Belt.Int.toString} is outside the array bounds.`,
        )->raise
      }
      if end < 0 || end > len - 1 {
        InvalidArgument(
          `The end index ${start->Belt.Int.toString} is outside the array bounds.`,
        )->raise
      }
      range(start, end)->map(inx => xx->Js.Array2.unsafe_get(inx))
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
  | Some(x) => once(x)
  }

let mapi = (xx, f) => xx->indexed->map(((x, inx)) => f(x, inx))

let take = (xx, count) => {
  switch count {
  | 0 => empty
  | n if n < 0 =>
    InvalidArgument(
      `take requires a count of 0 or more. You requested ${count->Belt.Int.toString}`,
    )->raise
  | count =>
    let rec go = (xx, count) =>
      (. ()) =>
        switch count {
        | 0 => End
        | count =>
          switch xx->nextNode {
          | End => End
          | Next(x, xx) => Next(x, go(xx, count - 1))
          }
        }
    go(xx, count)
  }
}

let headTails = xx =>
  unfold(xx, xx => xx->headTail->Option.flatMap(((_, xx) as ht) => Some(ht, xx)))

let snd = ((_, b)) => b

let drop = (xx, count) =>
  switch count {
  | 0 => xx
  | n if n < 0 =>
    InvalidArgument(
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

let filteri = (xx, f) => xx->indexed->filter(((x, inx)) => f(x, inx))->map(((v, _)) => v)

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

let filterMapi = (xx, f) =>
  xx->indexed->map(((x, inx)) => f(x, inx))->filter(Option.isSome)->map(Option.getUnsafe)

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
      | Next((x, inx), xx) => {
          let sum = f(~sum, ~val=x, ~inx)
          Next(sum, go(xx, sum))
        }
      }
  concat(once(zero), go(xx->indexed, zero))
}

let scan = (xx, zero, f) => scani(xx, ~zero, (~sum, ~val, ~inx as _) => f(sum, val))

let rec sortedMerge = (xx, yy, cmp) => {
  (. ()) =>
    switch (xx(.), yy(.)) {
    | (End, Next(_, _) as yy) => yy
    | (Next(_, _) as xx, End) => xx
    | (Next(x, xx), Next(y, yy)) => {
        let order = cmp(x, y)
        if order <= 0 {
          Next(x, sortedMerge(xx, concat(y->once, yy), cmp))
        } else {
          Next(y, sortedMerge(concat(x->once, xx), yy, cmp))
        }
      }
    | (End, End) => End
    }
}

let intersperseWith = (xx, separator) => {
  let rec beforeEach = (xx, separator) =>
    (. ()) => {
      switch xx->nextNode {
      | End => End
      | Next(x, xx) => Next(separator(), cons(x, beforeEach(xx, separator)))
      }
    }
  (. ()) =>
    switch xx->nextNode {
    | End => End
    | Next(x, xx) => Next(x, beforeEach(xx, separator))
    }
}

let intersperse = (xx, separator) => xx->intersperseWith(() => separator)

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

let allPairs = (xx, yy) => {
  let yy = yy->cache
  xx->flatMap(x => yy->map(y => (x, y)))
}

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
    InvalidArgument(
      `chunkByyize requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  xx
  ->map(i => Some(i))
  ->concat(repeat(length - 1, None))
  ->scani(~zero=[], (~sum, ~val, ~inx) => {
    switch val {
    | None => sum
    | Some(value) =>
      switch mod(inx, length) {
      | 0 => [value]
      | _ =>
        sum->Js.Array2.push(value)->ignore
        sum
      }
    }
  })
  ->filteri((_, inx) => mod(inx, length) == 0)
  ->drop(1)
}

let window = (xx, length) => {
  if length <= 0 {
    InvalidArgument(
      `windowed requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  xx
  ->scani(~zero=[], (~sum, ~val, ~inx as _) => {
    if Js.Array2.length(sum) >= length {
      sum->Js.Array2.shift->ignore
    }
    sum->Js.Array2.push(val)->ignore
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

let reducei = (xx, ~zero, concat) =>
  xx->indexed->reduce(zero, (sum, (val, inx)) => concat(~sum, ~val, ~inx))

let join = (xx, separator) =>
  switch separator->Js.String2.length {
  | 0 => xx
  | _ => xx->intersperse(separator)
  }->reduce("", (total, i) => total ++ i)

let last = xx => xx->reduce(None, (_, x) => Some(x))

let toArray = xx =>
  xx->reduce([], (xx, i) => {
    xx->Js.Array2.push(i)->ignore
    xx
  })

let forEachi = (xx, f) => xx->indexed->forEach(((x, inx)) => f(x, inx))

let some = (xx, f) => xx->find(f)->Option.isSome

let every = (xx, f) => xx->find(i => !f(i))->Option.isNone

let findMapi = (xx, f) =>
  xx->mapi((x, inx) => f(x, inx))->find(Option.isSome(_))->Option.map(Option.getUnsafe)

let findMap = (xx, f) => findMapi(xx, (x, _) => f(x))

let rec map2 = (xx, yy, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    Extras__Option.map2(xx, yy, ((x, xx), (y, yy)) => Next(
      f(x, y),
      map2(xx, yy, f),
    ))->Option.getWithDefault(End)
  }

let rec map3 = (xx, yy, zz, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    let zz = zz->next
    OptionEx.map3(xx, yy, zz, ((x, xx), (y, yy), (z, zz)) => Next(
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
    OptionEx.map4(xx, yy, zz, qq, ((x, xx), (y, yy), (z, zz), (q, qq)) => Next(
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
    OptionEx.map5(xx, yy, zz, qq, mm, ((x, xx), (y, yy), (z, zz), (q, qq), (m, mm)) => Next(
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
  map2(xx, yy, (x, y) =>
    switch (x, y) {
    | (None, None) => true
    | (Some(x), Some(y)) => eq(x, y)
    | _ => false
    }
  )->every(i => i)
}

let compare = (xx, yy, cmp) => {
  let xx = xx->map(x => Some(x))->endWith(None)
  let yy = yy->map(y => Some(y))->endWith(None)
  map2(xx, yy, (x, y) =>
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

let exactlyOne = xx =>
  switch xx->headTail {
  | None => None
  | Some(x, xx) =>
    switch xx->isEmpty {
    | true => Some(x)
    | false => None
    }
  }

let isSortedBy = (xx, cmp) => xx->pairwise->every(((a, b)) => cmp(a, b) <= 0)

let windowBehind = (xx, size) => {
  if size <= 0 {
    InvalidArgument(`windowBehind requires a size greater than zero.`)->raise
  }
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

let windowAhead = (xx, size) => {
  if size <= 0 {
    InvalidArgument(`windowAhead requires a size greater than zero.`)->raise
  }
  xx
  ->map(i => Some(i))
  ->concat(repeat(size - 1, None))
  ->scani(~zero=[], (~sum, ~val as i, ~inx) => {
    if inx >= size {
      sum->Js.Array2.shift->ignore
    }
    i->Option.forEach(i => sum->Js.Array2.push(i)->ignore)
    sum
  })
  ->drop(size)
}

let everyOk = xx => {
  xx
  ->scan(Ok(empty), (sum, x) =>
    switch x {
    | Ok(ok) => sum->Result.map(oks => concat(oks, once(ok)))
    | Error(_) as err => err
    }
  )
  ->takeUntil(Result.isError(_))
  ->last
  ->Option.getUnsafe
}

let everySome = xx => {
  xx
  ->scan(Some(empty), (sum, x) =>
    switch x {
    | Some(ok) => sum->Option.map(oks => concat(oks, once(ok)))
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

let consume = xx => xx->forEach(_ => ())

let orElse = (xx, yy) =>
  (. ()) => {
    switch xx->nextNode {
    | End => yy->nextNode
    | Next(_, _) as nxt => nxt
    }
  }

let reverse = xx =>
  delay(() => {
    let xx = xx->toArray
    switch xx->Js.Array2.length {
    | 0 => empty
    | _ => xx->fromArray(~start=xx->Js.Array2.length - 1, ~end=0)
    }
  })

let sortBy = (xx, compare) =>
  delay(() => {
    let xx = xx->toArray
    xx->Js.Array2.sortInPlaceWith(compare)->ignore
    xx->fromArray
  })

/**
`distribute(source, divider)` iterates through each item in `source` and returns
the sequences that result when `divider` is inserted before that item, and also
after the last item. If `source` is empty, returns a sequence consisting only of
`divider`.

```
[1, 2]->distribute(8) // [[8,1,2], [1,8,2], [1,2,8]]
[1]->distribute(8) // [[8,1], [1,8]]
[]->distribute(8) // [[8]]
```
*/
let distribute = (xx, item) => {
  let go = (pre, suf) =>
    unfold((pre, suf), ((pre, suf)) =>
      switch suf->headTail {
      | None => None
      | Some(x, xx) => {
          let yield = pre->endWith(x)->endWith(item)->concat(xx)
          let next = (pre->endWith(x), xx)
          Some(yield, next)
        }
      }
    )
  go(empty, xx)->startWith(cons(item, xx))
}

let (combinations, permutations) = {
  let helper = (xx, maxSize, f) => {
    if maxSize <= 0 {
      InvalidArgument(`Size must be 1 or more. You asked for ${maxSize->Belt.Int.toString}.`)->raise
    }
    unfold((empty, xx), ((sum, xx)) =>
      switch xx->headTail {
      | None => None
      | Some(x, xx) => {
          let next = {
            let xOnly = (1, x->once)
            let xWithSum = sum->flatMap(((size, xx)) =>
              switch size < maxSize {
              | true => f(x, xx)->map(xx => (size + 1, xx))
              | false => empty
              }
            )
            cons(xOnly, xWithSum)
          }
          Some(next, (concat(sum, next), xx))
        }
      }
    )->flatten
  }
  let permutations = (xx, maxSize) => helper(xx, maxSize, (x, xx) => distribute(xx, x))
  let combinations = (xx, maxSize) => helper(xx, maxSize, (x, xx) => once(cons(x, xx)))
  (combinations, permutations)
}
