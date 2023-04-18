module T = Extras__Test
module TR = Extras__Trampoline
module Array = Belt.Array

let factorial = n => {
  let rec go = (total, n) =>
    switch n {
    | 1 => TR.resolve(total)
    | n => TR.work(() => go(total * n, n - 1))
    }
  let initialState = TR.work(() => go(1, n))
  TR.solve(initialState)
}

let factorialTests =
  [(1, 1), (2, 2), (3, 6), (4, 24), (12, 479001600)]->Array.map(((n, total)) =>
    T.make(
      ~category="Functions",
      ~title="Trampoline (factorial)",
      ~expectation=`${n->Belt.Int.toString}! == ${total->Belt.Int.toString}`,
      ~predicate=() => factorial(n) == total,
    )
  )

let decrement = n => {
  let rec go = current =>
    switch current == 0 {
    | true => TR.resolve(current)
    | false => TR.work(() => go(current - 1))
    }
  let initialState = TR.work(() => go(n))
  TR.solve(initialState)
}

let decrementTests =
  [10, 100, 999999]->Array.map(n =>
    T.make(
      ~category="Functions",
      ~title="Trampoline (decrement)",
      ~expectation=`Can decrement from ${n->Belt.Int.toString}`,
      ~predicate=() => decrement(n) == 0,
    )
  )

let tests = [factorialTests, decrementTests]->Array.flatMap(i => i)
