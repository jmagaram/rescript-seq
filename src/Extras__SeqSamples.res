module Seq = Extras__Seq
module Option = Belt.Option
let intToString = Belt.Int.toString

// Wrap any array in a sequence and then use all the Seq functions to filter,
// map, reduce, and analyze it. This process does NOT copy the array; it just
// creates a lightweight function to iterate through its elements. When you're
// done, call `toArray`.

let arr = ["w", "v", "q", "s", "p", "x"]
let s1 = arr->Seq.fromArray
let s2 = arr->Seq.fromArray(~start=2)
let s3 = arr->Seq.fromArray(~end=4)

/**
Creating simple sequences.
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
      Js.log(`Saw ${n->intToString}. Interesting!`)
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
