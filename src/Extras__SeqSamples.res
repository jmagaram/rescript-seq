module Seq = Extras__Seq
module Option = Belt.Option
let intToString = Belt.Int.toString

// Wrap any array in a sequence and then have access to all the Seq functions to
// filter, map, reduce, and analyze it. When you're done, call `toArray`.

let arr = ["w", "v", "q", "s", "p", "x"]
let s1 = arr->Seq.fromArray
let s2 = arr->Seq.fromArray(~start=2)
let s3 = arr->Seq.fromArray(~end=4)

/**
Examples of constructing simple sequences.
*/
let aa = Seq.range(1, 100)
let bb = Seq.iterate(2, i => i * 2)->Seq.takeWhile(i => i < 1000)
let cc = Seq.foreverWith(() => Js.Math.random())
let dd = [1, 3, 7, 2]->Seq.fromArray->Seq.cycle
let ee = "banana"->Seq.characters->Seq.intersperse(",")
let ff = Seq.repeat(100, "x")->Seq.toArray
let gg = Seq.rangeMap(99, 1, i => i * 3)
let hh = Seq.singleton("Hello!")

/**
Pointless number crunching to see how it flows.
*/
let nums =
  Seq.range(1, 999_999)
  ->Seq.drop(33)
  ->Seq.map(n => mod(n * 3, 7))
  ->Seq.pairwise
  ->Seq.filterMap(((a, b)) => a < b ? Some(a + b) : None)
  ->Seq.takeAtMost(623)
  ->Seq.tap(n => {
    if mod(n, 100) == 0 {
      Js.log(`Saw ${n->Belt.Int.toString}. Interesting!`)
    }
  })
  ->Seq.flatMap(n => Seq.rangeMap(1, n, i => i * 2))
  ->Seq.last

/**
Calculates the infinite fibonacci sequence and returns the specified count of
items in an array.
*/
let fibonacci = count =>
  Seq.unfold((0, 1), ((a, b)) => a + b <= 100 ? Some(a + b, (b, a + b)) : None)
  ->Seq.prepend([0, 1]->Seq.fromArray)
  ->Seq.takeAtMost(count)
  ->Seq.toArray

/**
There is a built-in `zip` function that combines corresponding items from two
sequences. It stops when either one of the input sequences ends. But what if we
want to do a zipLongest? 
*/
let zipLongest = (xx, yy) => {
  let optionForever = xx => xx->Seq.map(x => Some(x))->Seq.concat(None->Seq.forever)
  let xx = xx->optionForever
  let yy = yy->optionForever
  Seq.zip(xx, yy)->Seq.takeWhile(((x, y)) => x->Option.isSome || y->Option.isSome)
}

/**
Calculates the binary digits in a number.
*/
let binary = n =>
  Seq.unfold(n, value => value > 0 ? Some(mod(value, 2), value / 2) : None)
  ->Seq.toArray
  ->Belt.Array.reverse
  ->Seq.fromArray

/**
Divides an array into non-overlapping chunks of arbitrary size.
*/
let chunk = (xx, size) =>
  Seq.unfold(0, i =>
    switch xx->Js.Array2.slice(~start=i, ~end_=i + size) {
    | [] => None
    | chunk => Some(chunk, i + size)
    }
  )

/**
Calculates the local minimum points in an array, converts each to a string, and
concatenates them with a comma between each.
*/
module Point = {
  type t = (int, int)
  let x = ((x, _)) => x
  let y = ((_, y)) => y
  let toString = ((x, y)) => `(${x->intToString}, ${y->intToString})`
}
let localMinimums = points =>
  points
  ->Seq.fromArray
  ->Seq.window(3)
  ->Seq.filterMap(pp =>
    pp[1]->Point.y < pp[0]->Point.y && pp[1]->Point.y < pp[2]->Point.y ? Some(pp[1]) : None
  )
  ->Seq.map(Point.toString)
  ->Seq.toOption
  ->Option.map(pp => pp->Seq.intersperse(", ")->Seq.joinString)
  ->Option.getWithDefault("There are no local minimums.")

/**
Validates an array of "docs" lazily. If all are `Ok` prints out each of their
titles. Otherwise prints the title of the first invalid doc. This is essentially
converting an array of results to a result of array.
*/
let validate = (docs, isValidEmail) =>
  switch docs
  ->Seq.fromArray
  ->Seq.map(doc =>
    switch doc["email"]->isValidEmail {
    | false => Error(`The email is invalid for document: ${doc["title"]}`)
    | true => Ok(doc["title"]->Js.String2.trim)
    }
  )
  ->Seq.allOk {
  | Ok(titles) => titles->Seq.forEach(t => Js.log(`Is valid: ${t}`))
  | Error(title) => Js.log(`Document with title ${title} is not valid.`)
  }

/**
Computes the running total of integers in an array. `scan` is conceptually
similar to `reduce` but returns intermediate results. For example, the running
total of [1, 2, 3, 4] is [0, 1, 3, 6, 10] 
*/
let runningTotal = nums => nums->Seq.fromArray->Seq.scan(0, (sum, i) => sum + i)

type match<'a> =
  | Empty
  | Singleton('a)
  | HeadTail('a, Seq.t<'a>)

let match = xx =>
  switch xx->Seq.headTail {
  | None => Empty
  | Some(x, xx) =>
    switch xx->Seq.toOption {
    | None => Singleton(x)
    | Some(_) => HeadTail(x, xx)
    }
  }

let rec combinations = xx => {
  open Seq
  switch xx->headTail {
  | None => empty->singleton
  | Some(x, xx) => combinations(xx)->flatMap(xx => concat(xx->singleton, cons(x, xx)->singleton))
  }
}

let rec combos = (xx, max) => {
  open Seq
  if max == 0 {
    empty->singleton
  } else if max == 1 {
    xx->map(singleton)
  } else {
    switch xx->headTail {
    | None => empty->singleton
    | Some(x, xx) => {
        let m = combos(xx, max)
        let n = combos(xx, max - 1)->map(xx => cons(x, xx))->orElse(x->singleton->singleton)
        concat(m, n)
      }
    }
  }
}

// let rec combos = (xx, max) => {
//   open Seq
//   switch max == 0 {
//   | true => empty->singleton
//   | false =>
//     switch xx->headTail {
//     | None => empty->singleton
//     | Some(x, xx) =>
//       switch max == 1 {
//       | true => concat(x->singleton, xx)
//       | false => combos(xx, max - 1)->flatMap(xx => concat(xx->singleton, cons(x, xx)->singleton))
//       }
//     }
//   }
// }

// let combos = (~xx, ~minLength, ~maxLength) => {
//   open Seq
//   let rec go = (~xx, ~prefixLength) => {
//     switch prefixLength >= maxLength {
//     | true => empty->singleton
//     | false =>
//       switch xx->headTail {
//       | None => empty->singleton
//       | Some(x, xx) =>
//         switch prefixLength < maxLength {
//         | true => {
//             let withoutHead = go(~xx, ~prefixLength)
//             withoutHead->flatMap(xx =>
//               concat(xx->singleton, prefixLength < maxLength ? cons(x, xx)->singleton : Seq.empty)
//             )
//           }
//         | false =>
//           go(~xx, ~prefixLength=prefixLength + 1)->flatMap(xx =>
//             concat(xx->singleton, cons(x, xx)->singleton->filter(_ => prefixLength < maxLength))
//           )
//         }
//       }
//     }
//   }
//   go(~xx, ~prefixLength=0)
// }
