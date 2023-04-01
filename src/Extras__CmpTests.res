module T = Extras__Test
module C = Extras__Cmp

type person = {name: string, age: int}
let tests = [
  T.make(~category="Cmp", ~title="fromMap", ~expectation="", ~predicate=() => {
    let target = C.fromMap(i => i.age, C.int)
    let bob = {name: "bob", age: 3}
    let sue = {name: "sue", age: 8}
    target(bob, sue) == -1
    // target(bob, bob)->T.expect->T.Expect.toEqual(0)
  }),
]
// T.test("fromFloatResult", _ => {
//   let f = (x: int, y: int) => x < y ? -1.0 : x > y ? 1.0 : 0.0
//   let target = Cmp.fromFloatResult(f)
//   target(3, 9)->T.expect->T.Expect.toEqual(-1)
//   target(3, 3)->T.expect->T.Expect.toEqual(0)
// })

// T.test("fromMap", _ => {
//   let target = Cmp.fromMap(i => i.age, Cmp.int)
//   let bob = {name: "bob", age: 3}
//   let sue = {name: "sue", age: 8}
//   target(bob, sue)->T.expect->T.Expect.toEqual(-1)
//   target(bob, bob)->T.expect->T.Expect.toEqual(0)
// })

// T.test("int instance", _ => {
//   Cmp.int(1, 2)->T.expect->T.Expect.toEqual(-1)
//   Cmp.int(1, 1)->T.expect->T.Expect.toEqual(0)
// })

// T.test("float instance", _ => {
//   Cmp.float(1.0, 2.0)->T.expect->T.Expect.toEqual(-1)
//   Cmp.float(1.0, 1.0)->T.expect->T.Expect.toEqual(0)
// })

// T.test("bool instance", _ => {
//   Cmp.bool(true, false)->T.expect->T.Expect.toEqual(1)
//   Cmp.bool(true, true)->T.expect->T.Expect.toEqual(0)
//   Cmp.bool(false, false)->T.expect->T.Expect.toEqual(0)
// })
