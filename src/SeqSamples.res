module Seq = Extras__Seq

let emails = ["justin@google.com", "mike@", "bob@gmail.com"]

let a =
  emails
  ->Seq.fromArray
  ->Seq.map(i => i->Js.String2.includes("@") ? Ok(i) : Error((i, "Invalid email!")))
  ->Seq.allOk

let x =
  Seq.infinite(() => Js.Math.random())
  ->Seq.pairwise
  ->Seq.takeAtMost(1000)
  ->Seq.toArray
  ->Seq.fromArray

let numbers =
  Seq.infinite(() => Js.Math.random())
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

// ["apple", "banana", "orange", "pears"]->Seq.fromArray->S

let intToString = Belt.Int.toString
type point = {x: int, y: int}

let localMinimums = points =>
  points
  ->Seq.fromArray
  ->Seq.window(3)
  ->Seq.filterMap(pp => pp[1].y < pp[0].y && pp[1].y < pp[2].y ? Some(pp[1]) : None)
  ->Seq.map(p => `(${p.x->intToString},${p.y->intToString})`)
  ->Seq.intersperse(",")
  ->Seq.joinString

let validateDocs = (documents, validate) =>
  switch documents
  ->Seq.fromArray
  ->Seq.filter(doc => doc["status"] == "unprocessed")
  ->Seq.map(validate)
  ->Seq.allOk {
  | Ok(docs) => docs->Seq.map(doc => doc["title"])->Seq.forEach(Js.log)
  | Error(err) => Js.log(`First error: ${err}`)
  }

let fibs = count =>
  Seq.unfold((0, 1), ((a, b)) => a + b <= 100 ? Some(a + b, (b, a + b)) : None)
  ->Seq.prepend([0, 1]->Seq.fromArray)
  ->Seq.takeAtMost(count)
  ->Seq.map(Belt.Int.toString)
  ->Seq.intersperse(", ")
  ->Seq.joinString
