module T = Extras__Test
module S = Extras__Seq

let areEqual = (~title, ~a, ~b) =>
  T.make(~category="Seq", ~title, ~expectation="", ~predicate=() => {
    let a = a()->S.toArray
    if a != b {
      Js.Console.log(`===== NOT EQUAL : ${title} =====`)
      Js.Console.log(`A: ${a->Js.Array2.toString}`)
      Js.Console.log(`B: ${b->Js.Array2.toString}`)
    }
    a == b
  })

let tests = [
  areEqual(~title="singleton", ~a=() => S.singleton(3), ~b=[3]),
  areEqual(~title="empty", ~a=() => S.empty, ~b=[]),
]
