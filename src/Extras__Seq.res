module Option = Belt.Option
module Result = Belt.Result
module OptionEx = Extras__Option

exception InvalidArgument(string)

/**
This is the type definition of a sequence. It is basically a function that
returns either `End`, meaning there is nothing left in the sequence, or `Next`,
indicating the next item. It uses uncurried mode since the resultant JavaScript
looked a bit simpler to me. This is an abstract type in case we want to change
the implementation later to use an `option` perhaps. Or maybe generate more
performant code for sequences that wrap arrays. Note that the `unfold` function
is nearly identical to this implementation, so not much flexibility is lost.
*/
type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | End
  | Next('a, t<'a>)

let delay = generator => (. ()) => generator()(.)

let empty = (. ()) => End

let once = x => (. ()) => Next(x, empty)

let onceWith = f => (. ()) => Next(f(), empty)

let cons = (x, xx) => (. ()) => Next(x, xx)

/**
Internal helper function to generate the next node in a sequence. It is
sometimes simpler to use this in pipeline mode because if you put a `(.)` at the
end it latches on to the very last item in the sequence rather than the result
of everything that came before.
*/
@inline
let nextNode = (xx: t<'a>) => xx(.)

/**
This tries to consume the first item in the sequence and returns `None` if the
sequence is empty or a `Some` with the head and tail. This is the same as
`uncons` but with a different name. It could be named `match` perhaps.
*/
let headTail = xx =>
  switch xx->nextNode {
  | End => None
  | Next(x, xx) => Some(x, xx)
  }

/**
`findNode(source, predicate)` is a foundation method for many of the functions
in this library. This must not compile to recursive JavaScript or stack
overflows can result. This function consumes at least one item from the source.
*/
let rec findNode = (xx, f) =>
  switch xx->nextNode {
  | End => End
  | Next(x, xx) as nxt =>
    switch f(x) {
    | true => nxt
    | false => findNode(xx, f)
    }
  }

let find = (xx, f) =>
  switch xx->findNode(f) {
  | End => None
  | Next(x, _) => Some(x)
  }

let rec concat = (xx, yy) =>
  (. ()) => {
    switch xx->nextNode {
    | End => yy->nextNode
    | Next(x, xx) => Next(x, concat(xx, yy))
    }
  }

@inline let prepend = (xx, yy) => concat(yy, xx)

let rec flatMap = (xx, f) =>
  (. ()) =>
    switch xx->nextNode {
    | End => End
    | Next(x, xx) => concat(f(x), flatMap(xx, f))(.)
    }

let rec flatten = xxx =>
  (. ()) =>
    switch xxx->nextNode {
    | End => End
    | Next(xx, xxx) => concat(xx, flatten(xxx))->nextNode
    }

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

let indexed = xx => {
  let rec go = (xx, index) =>
    (. ()) =>
      switch xx->nextNode {
      | End => End
      | Next(x, xx) => Next((x, index), go(xx, index + 1))
      }
  go(xx, 0)
}

let rec forEach = (xx, f) =>
  switch xx->headTail {
  | None => ()
  | Some(x, xx) => {
      f(x)
      forEach(xx, f)
    }
  }

let forEachi = (xx, f) => xx->indexed->forEach(((x, inx)) => f(x, inx))

let rec unfold = (seed, f) =>
  (. ()) =>
    switch f(seed) {
    | None => End
    | Some(x, seed) => Next(x, unfold(seed, f))
    }

let init = (count, f) => unfold(0, i => i < count ? Some(f(i), i + 1) : None)

let replicate = (count, value) => unfold(0, i => i < count ? Some(value, i + 1) : None)

let rec forever = value => (. ()) => Next(value, forever(value))

let rec foreverWith = f => (. ()) => Next(f(), foreverWith(f))

let replicateWith = (count, value) => unfold(1, i => i <= count ? Some(value(), i + 1) : None)

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

let cycle = xx =>
  (. ()) =>
    switch xx->headTail {
    | None => End
    | Some(x, xx') => {
        let rec cycleNonEmpty = xx => (. ()) => concat(xx, cycleNonEmpty(xx))->nextNode
        cons(x, xx')->concat(cycleNonEmpty(xx))->nextNode
      }
    }

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
  if count < 0 {
    InvalidArgument(
      `take requires a count of 0 or more. You requested ${count->Belt.Int.toString}`,
    )->raise
  }
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

let headTails = xx =>
  unfold(xx, xx => xx->headTail->Option.flatMap(((_, xx) as ht) => Some(ht, xx)))

let rec dropEager = (xx, count) =>
  switch count {
  | 0 => xx
  | count =>
    switch xx->headTail {
    | None => empty
    | Some(_, xx) => dropEager(xx, count - 1)
    }
  }

let drop = (xx, count) =>
  switch count {
  | 0 => xx
  | count if count < 0 =>
    InvalidArgument(
      `'drop' requires a count of zero or more but you asked for ${count->Belt.Int.toString}`,
    )->raise
  | count => delay(() => dropEager(xx, count))
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

let rec takeWhile = (xx, f) =>
  (. ()) => {
    switch xx->nextNode {
    | End => End
    | Next(x, xx) =>
      switch f(x) {
      | true => Next(x, takeWhile(xx, f))
      | false => End
      }
    }
  }

let rec takeUntil = (xx, f) =>
  (. ()) => {
    switch xx->nextNode {
    | End => End
    | Next(x, xx) =>
      switch f(x) {
      | true => Next(x, empty)
      | false => Next(x, takeUntil(xx, f))
      }
    }
  }

let filterMapi = (xx, f) => {
  let rec go = xx =>
    switch xx->headTail {
    | None => None
    | Some((x, index), xx) =>
      switch f(x, index) {
      | None => go(xx)
      | Some(x) => Some(x, xx)
      }
    }
  unfold(xx->indexed, go)
}

let filterMap = (xx, f) => {
  let rec go = xx =>
    switch xx->headTail {
    | None => None
    | Some(x, xx) =>
      switch f(x) {
      | None => go(xx)
      | Some(x) => Some(x, xx)
      }
    }
  unfold(xx, go)
}

let filterSome = xx => {
  let rec go = xx =>
    switch xx->headTail {
    | None => None
    | Some(x, xx) =>
      switch x {
      | None => go(xx)
      | Some(x) => Some(x, xx)
      }
    }
  unfold(xx, go)
}

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

module Cache = {
  type t<'a> = (. unit) => 'a

  type toLazy<'a> = t<'a> => Lazy.t<'a>
  let toLazy: toLazy<'a> = (f: t<'a>) => {
    let g = () => f(.)
    Lazy.from_fun(g)
  }

  type fromLazy<'a> = Lazy.t<'a> => t<'a>
  let fromLazy: fromLazy<'a> = f => (. ()) => Lazy.force(f)

  type make<'a> = t<'a> => t<'a>
  let make: make<'a> = f => f->toLazy->fromLazy
}

let rec cache = xx =>
  Cache.make((. ()) =>
    switch xx->nextNode {
    | End => End
    | Next(x, xx) => Next(x, cache(xx))
    }
  )

let allPairs = (xx, yy) => {
  let yy = yy->cache
  xx->flatMap(x => yy->map(y => (x, y)))
}

let dropUntil = (xx, f) =>
  (. ()) =>
    xx
    ->headTails
    ->find(((x, _)) => f(x))
    ->Option.map(((x, xx)) => Next(x, xx))
    ->Option.getWithDefault(End)

let dropWhile = (xx, f) =>
  (. ()) =>
    xx
    ->headTails
    ->find(((x, _)) => false == f(x))
    ->Option.map(((x, xx)) => Next(x, xx))
    ->Option.getWithDefault(End)

let chunkBySize = (xx, length) => {
  if length <= 0 {
    InvalidArgument(
      `chunkByyize requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  xx
  ->map(i => Some(i))
  ->concat(replicate(length - 1, None))
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

let fold = (xx, zero, concat) => {
  let sum = ref(zero)
  xx->forEach(x => sum := concat(sum.contents, x))
  sum.contents
}

let foldUntil = (xx, zero, concat, predicate) => {
  let rec go = (xx, sum) =>
    switch predicate(sum) {
    | true => sum
    | false =>
      switch xx->headTail {
      | None => sum
      | Some(x, xx) => go(xx, concat(sum, x))
      }
    }
  go(xx, zero)
}

let foldWhile = (xx, zero, concat, predicate) => {
  let rec go = (xx, sum) =>
    switch xx->headTail {
    | None => Some(sum)
    | Some(x, xx) => {
        let sum' = concat(sum, x)
        switch predicate(sum') {
        | true => go(xx, sum')
        | false => Some(sum)
        }
      }
    }
  switch predicate(zero) {
  | false => None
  | true => go(xx, zero)
  }
}

let foldi = (xx, ~zero, concat) =>
  xx->indexed->fold(zero, (sum, (val, inx)) => concat(~sum, ~val, ~inx))

let reduce = (xx, concat) =>
  switch xx->headTail {
  | None => None
  | Some(x, xx) => {
      let sum = ref(x)
      xx->forEach(x => sum := concat(sum.contents, x))
      Some(sum.contents)
    }
  }

let reduceUntil = (xx, concat, predicate) => {
  let rec go = (sum, xx) => {
    switch predicate(sum) {
    | true => Some(sum)
    | false =>
      switch xx->headTail {
      | None => Some(sum)
      | Some(x, xx) => go(concat(sum, x), xx)
      }
    }
  }
  switch xx->headTail {
  | None => None
  | Some(x, xx) => go(x, xx)
  }
}

let reduceWhile = (xx, concat, predicate) => {
  let rec go = (sum, xx) => {
    switch xx->headTail {
    | None => Some(sum)
    | Some(x, xx) => {
        let sum' = concat(sum, x)
        switch predicate(sum') {
        | true => go(sum', xx)
        | false => Some(sum)
        }
      }
    }
  }
  switch xx->headTail {
  | None => None
  | Some(x, xx) =>
    switch predicate(x) {
    | true => go(x, xx)
    | false => None
    }
  }
}

let join = (xx, separator) =>
  switch separator->Js.String2.length {
  | 0 => xx
  | _ => xx->intersperse(separator)
  }->fold("", (total, i) => total ++ i)

let last = xx => xx->reduce((_, i) => i)

let toArray = xx =>
  xx->fold([], (xx, i) => {
    xx->Js.Array2.push(i)->ignore
    xx
  })

let some = (xx, f) => xx->find(f)->Option.isSome

let every = (xx, f) => xx->find(i => !f(i))->Option.isNone

let findMapi = (xx, f) => {
  let found = xx->indexed->map(((x, inx)) => f(x, inx))->find(Option.isSome)
  switch found {
  | None => None
  | Some(x) => x
  }
}

let findMap = (xx, f) => {
  let found = xx->map(f)->find(Option.isSome)
  switch found {
  | None => None
  | Some(x) => x
  }
}

let rec map2 = (xx, yy, f) =>
  (. ()) => {
    let xx = xx->headTail
    let yy = yy->headTail
    OptionEx.map2(xx, yy, ((x, xx), (y, yy)) => Next(
      f(x, y),
      map2(xx, yy, f),
    ))->Option.getWithDefault(End)
  }

let rec map3 = (xx, yy, zz, f) =>
  (. ()) => {
    let xx = xx->headTail
    let yy = yy->headTail
    let zz = zz->headTail
    OptionEx.map3(xx, yy, zz, ((x, xx), (y, yy), (z, zz)) => Next(
      f(x, y, z),
      map3(xx, yy, zz, f),
    ))->Option.getWithDefault(End)
  }

let rec map4 = (xx, yy, zz, qq, f) =>
  (. ()) => {
    let xx = xx->headTail
    let yy = yy->headTail
    let zz = zz->headTail
    let qq = qq->headTail
    OptionEx.map4(xx, yy, zz, qq, ((x, xx), (y, yy), (z, zz), (q, qq)) => Next(
      f(x, y, z, q),
      map4(xx, yy, zz, qq, f),
    ))->Option.getWithDefault(End)
  }

let rec map5 = (xx, yy, zz, qq, mm, f) =>
  (. ()) => {
    let xx = xx->headTail
    let yy = yy->headTail
    let zz = zz->headTail
    let qq = qq->headTail
    let mm = mm->headTail
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
  let xx = xx->map(x => Some(x))->concat(None->once)
  let yy = yy->map(y => Some(y))->concat(None->once)
  map2(xx, yy, (x, y) =>
    switch (x, y) {
    | (None, None) => true
    | (Some(x), Some(y)) => eq(x, y)
    | _ => false
    }
  )->every(i => i)
}

let compare = (xx, yy, cmp) => {
  let xx = xx->map(x => Some(x))->concat(None->once)
  let yy = yy->map(y => Some(y))->concat(None->once)
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

let length = xx => xx->fold(0, (sum, _) => sum + 1)

let isEmpty = xx =>
  switch xx->nextNode {
  | End => true
  | _ => false
  }

let tail = xx =>
  (. ()) =>
    switch xx->headTail {
    | None => End
    | Some(_, xx) => xx->nextNode
    }

let minBy = (xx, cmp) => xx->reduce((sum, i) => cmp(i, sum) < 0 ? i : sum)

let maxBy = (xx, cmp) => xx->reduce((sum, i) => cmp(i, sum) > 0 ? i : sum)

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
  ->concat(replicate(size - 1, None))
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
  let concat = (sum, i) =>
    switch sum {
    | Error(_) as error => error
    | Ok(oks) =>
      switch i {
      | Error(_) as error => error
      | Ok(ok) => Ok(oks->concat(ok->once))
      }
    }
  xx->foldUntil(Ok(empty), concat, Result.isError)
}

let everySome = xx => {
  let concat = (sum, i) =>
    switch sum {
    | None => None
    | Some(somes) =>
      switch i {
      | None => None
      | Some(some) => Some(somes->concat(some->once))
      }
    }
  xx->foldUntil(Some(empty), concat, Option.isNone)
}

let toOption = xx =>
  switch xx->nextNode {
  | End => None
  | Next(x, xx) => Some(cons(x, xx))
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
          let yield = pre->concat(x->once)->concat(item->once)->concat(xx)
          let next = (pre->concat(x->once), xx)
          Some(yield, next)
        }
      }
    )
  go(empty, xx)->concat(cons(item, xx)->once)
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

let toList = xx => xx->reverse->fold(list{}, Belt.List.add)
