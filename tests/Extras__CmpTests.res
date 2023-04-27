module T = Extras__Test
module C = Extras__Cmp
module A = Belt.Array

type person = {name: string, age: int}

let inOrder = (cmp, a, b) => cmp(a, b) == -1 && cmp(b, a) == 1
let isEqual = (cmp, a, b) => cmp(a, b) == 0 && cmp(b, a) == 0

let lessThanTests = (~small, ~big, ~cmp) => {
  let make = (~title, ~isTrue) =>
    T.fromPredicate(~category="Cmp", ~title, ~expectation="when different", () => isTrue)
  [
    make(~title="lt", ~isTrue=C.lt(cmp, small, big)),
    make(~title="lte", ~isTrue=C.lte(cmp, small, big)),
    make(~title="gt", ~isTrue=C.gt(cmp, big, small)),
    make(~title="gte", ~isTrue=C.gte(cmp, big, small)),
    make(~title="neq", ~isTrue=C.neq(cmp, big, small)),
    make(~title="eq", ~isTrue=C.eq(cmp, big, small) == false),
    make(~title="min", ~isTrue=C.min(cmp, big, small) == small),
    make(~title="max", ~isTrue=C.max(cmp, big, small) == big),
  ]
}

let areSameTests = (~a, ~b, ~cmp) => {
  let make = (~title, ~isTrue) =>
    T.fromPredicate(~category="Cmp", ~title, ~expectation="when same", () => isTrue)
  [
    make(~title="lt", ~isTrue=false == C.lt(cmp, a, b)),
    make(~title="lte", ~isTrue=C.lte(cmp, a, b)),
    make(~title="gt", ~isTrue=false == C.gt(cmp, a, b)),
    make(~title="gte", ~isTrue=C.gte(cmp, a, b)),
    make(~title="neq", ~isTrue=false == C.neq(cmp, a, b)),
    make(~title="eq", ~isTrue=C.eq(cmp, a, b)),
    make(~title="min", ~isTrue=C.min(cmp, a, b) == a),
    make(~title="max", ~isTrue=C.max(cmp, a, b) == a),
  ]
}

let otherTests = [
  T.fromPredicate(~category="Cmp", ~title="fromMap", ~expectation="", () => {
    let target = C.fromMap(i => i.age, C.int)
    let bob = {name: "bob", age: 3}
    let sue = {name: "sue", age: 8}
    inOrder(target, bob, sue)
  }),
  T.fromPredicate(~category="Cmp", ~title="fromFloatResult", ~expectation="", () => {
    let f = (x: int, y: int) => x < y ? -1.0 : x > y ? 1.0 : 0.0
    let target = C.fromFloatResult(f)
    inOrder(target, 3, 9) && isEqual(target, 3, 3)
  }),
  T.fromPredicate(~category="Cmp", ~title="int instance", ~expectation="", () => {
    inOrder(C.int, 1, 2) && isEqual(C.int, 1, 1)
  }),
  T.fromPredicate(~category="Cmp", ~title="float instance", ~expectation="", () => {
    inOrder(C.float, 1.0, 2.0) && isEqual(C.float, 1.0, 1.0)
  }),
  T.fromPredicate(~category="Cmp", ~title="bool instance", ~expectation="", () => {
    inOrder(C.bool, false, true) && isEqual(C.bool, false, false)
  }),
]

let tests =
  [
    lessThanTests(~small=1, ~big=99, ~cmp=C.int),
    lessThanTests(~small=1, ~big=99, ~cmp=C.int),
    lessThanTests(~small="a", ~big="b", ~cmp=C.string),
    areSameTests(~a=1.0, ~b=1.0, ~cmp=C.float),
    areSameTests(~a=1, ~b=1, ~cmp=C.int),
    areSameTests(~a="abc", ~b="abc", ~cmp=C.string),
    otherTests,
  ]->A.concatMany
