module T = Extras__Test
module C = Extras__Cmp

type person = {name: string, age: int}

let inOrder = (cmp, a, b) => cmp(a, b) == -1 && cmp(b, a) == 1
let isEqual = (cmp, a, b) => cmp(a, b) == 0 && cmp(b, a) == 0

let tests = [
  T.make(~category="Cmp", ~title="fromMap", ~expectation="", ~predicate=() => {
    let target = C.fromMap(i => i.age, C.int)
    let bob = {name: "bob", age: 3}
    let sue = {name: "sue", age: 8}
    inOrder(target, bob, sue)
  }),
  T.make(~category="Cmp", ~title="fromFloatResult", ~expectation="", ~predicate=() => {
    let f = (x: int, y: int) => x < y ? -1.0 : x > y ? 1.0 : 0.0
    let target = C.fromFloatResult(f)
    inOrder(target, 3, 9) && isEqual(target, 3, 3)
  }),
  T.make(~category="Cmp", ~title="int instance", ~expectation="", ~predicate=() => {
    inOrder(C.int, 1, 2) && isEqual(C.int, 1, 1)
  }),
  T.make(~category="Cmp", ~title="float instance", ~expectation="", ~predicate=() => {
    inOrder(C.float, 1.0, 2.0) && isEqual(C.float, 1.0, 1.0)
  }),
  T.make(~category="Cmp", ~title="bool instance", ~expectation="", ~predicate=() => {
    inOrder(C.bool, false, true) && isEqual(C.bool, false, false)
  }),
]
