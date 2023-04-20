open Belt
module Test = Extras__Test
module Ex = Extras
module Promise = Js.Promise2

let isLocalDevelopment = () => {
  try {
    let isLocal: bool = %raw(`process.env.NODE_ENV === "development"`)
    isLocal
  } catch {
  | _ => false
  }
}

let keywords = []
let onlyShowFailures = true
let filter = test => keywords->Js.Array2.every(word => test->Test.hasKeyword(word))
let throwIfAnyTestFails = !isLocalDevelopment()

let tests =
  [
    Extras__CmpTests.tests,
    Extras__ArrayTests.tests,
    Extras__OptionTests.tests,
    Extras__ResultTests.allTests,
    Extras__TaskTest.tests,
    Extras__NonEmptyArrayTests.tests,
    Extras__LiteralTests.tests,
    Extras__UnionTests.tests,
    Extras__PatternTests.tests,
    Extras__UnknownTests.tests,
    Extras__SeqTests.tests,
    Extras__TrampolineTests.tests,
  ]->Array.concatMany

Ex.Task.Result.make(
  ~promise=() => Ex.Test.runSuite(tests, ~filter, ~onlyShowFailures),
  ~onError=e => e,
)
->Ex.Task.map(i =>
  switch i {
  | Ok(s) if s.fail == 0 => None
  | Ok(s) if s.fail > 0 => Some(`Tests failed: ${s.fail->Int.toString}`)
  | _ => Some("Could not run the test suite, or an unexpected failure.")
  }
)
->Ex.Task.map(i => throwIfAnyTestFails ? i : None)
->Ex.Task.forEach(Option.forEach(_, msg => Js.Exn.raiseError(msg)))
->Ex.Task.toPromise
->ignore
