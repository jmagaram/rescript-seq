module T = Extras__Test
module F = Extras__Trampoline
module Array = Belt.Array

let factorial = n => {
  let rec go = (total, n) =>
    switch n {
    | 1 => F.Trampoline.resolve(total)
    | n => F.Trampoline.work(() => go(total * n, n - 1))
    }
  let initialState = F.Trampoline.work(() => go(1, n))
  F.Trampoline.solve(initialState)
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
    | true => F.Trampoline.resolve(current)
    | false => F.Trampoline.work(() => go(current - 1))
    }
  let initialState = F.Trampoline.work(() => go(n))
  F.Trampoline.solve(initialState)
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
