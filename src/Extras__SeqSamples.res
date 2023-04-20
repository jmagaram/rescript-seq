module Seq = Extras__Seq
module Option = Belt.Option

/**
Calculates the fibonacci sequence; continues forever.
*/
let fibonacci = () =>
  Seq.unfold((0, 1), ((a, b)) => a + b <= 100 ? Some(a + b, (b, a + b)) : None)->Seq.prepend(
    [0, 1]->Seq.fromArray,
  )

/**
Zips the sequences `xx` and `yy` of possibly different lengths into one sequence
of tuples. 
*/
let zipAll = (xx, yy) => {
  let optionForever = xx => xx->Seq.map(x => Some(x))->Seq.concat(None->Seq.forever)
  let xx = xx->optionForever
  let yy = yy->optionForever
  Seq.zip(xx, yy)->Seq.takeWhile(((x, y)) => x->Option.isSome || y->Option.isSome)
}

/**
Calculates the binary digits in a number.
*/
let binary = n =>
  Seq.unfold(n, i => i > 0 ? Some(mod(i, 2), i / 2) : None)->Seq.toArray->Belt.Array.reverse

/**
Divides an array into non-overlapping chunks of arbitrary size.
*/
let chunk = (xx, size) =>
  Seq.unfold(0, i =>
    switch xx->Js.Array2.slice(~start=i, ~end_=i + size) {
    | [] => None
    | chunk => Some(chunk, i + size)
    }
  )->Seq.toArray

type point = {x: int, y: int}

/**
Calculates the local minimum points in an array, converts each to a string, and
concatenates them with a comma between each.
*/
let localMinimums = points =>
  points
  ->Seq.fromArray
  ->Seq.window(3)
  ->Seq.filterMap(pp => pp[1].y < pp[0].y && pp[1].y < pp[2].y ? Some(pp[1]) : None)
  ->Seq.map(p => `(${p.x->Belt.Int.toString},${p.y->Belt.Int.toString})`)
  ->Seq.intersperse(",")
  ->Seq.joinString

let emails = ["justin@google.com", "mike@", "bob@gmail.com"]

let a =
  emails
  ->Seq.fromArray
  ->Seq.map(i => i->Js.String2.includes("@") ? Ok(i) : Error((i, "Invalid email!")))
  ->Seq.allOk

let x =
  Seq.foreverWith(() => Js.Math.random())
  ->Seq.pairwise
  ->Seq.takeAtMost(1000)
  ->Seq.toArray
  ->Seq.fromArray

let numbers =
  Seq.foreverWith(() => Js.Math.random())
  ->Seq.filter(i => i < 0.3)
  ->Seq.pairwise
  ->Seq.filter(((a, b)) => a < b)
  ->Seq.takeAtMost(1000)
  ->Seq.toArray

// Seq.range(1, 999_999)->Seq.filter(i => mod(i, 7) == 0)->Seq.map(i => `The number is `)

let words =
  "John Smith"
  ->Js.String2.split(" ")
  ->Seq.fromArray
  ->Seq.map(Js.String2.toUpperCase(_))
  ->Seq.intersperse("")

let validateDocs = (documents, validate) =>
  switch documents
  ->Seq.fromArray
  ->Seq.filter(doc => doc["status"] == "unprocessed")
  ->Seq.map(validate)
  ->Seq.allOk {
  | Ok(docs) => docs->Seq.map(doc => doc["title"])->Seq.forEach(Js.log)
  | Error(err) => Js.log(`First error: ${err}`)
  }

/**
Calculates the fibonacci sequence.
*/
let fibs = count =>
  Seq.unfold((0, 1), ((a, b)) => a + b <= 100 ? Some(a + b, (b, a + b)) : None)
  ->Seq.prepend([0, 1]->Seq.fromArray)
  ->Seq.takeAtMost(count)
  ->Seq.map(Belt.Int.toString)
  ->Seq.intersperse(", ")
  ->Seq.joinString
