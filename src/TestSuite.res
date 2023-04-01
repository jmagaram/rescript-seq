open Belt

let filter = _ => true

let summary =
  await [Extras__ArrayTests.tests, Extras__OptionTests.tests, Extras__ResultTests.allTests]
  ->Array.concatMany
  ->Extras__Test.runSuite(~filter)

if summary.fail > 0 {
  Js.Exn.raiseError(`Failed tests: ${summary.fail->Int.toString}`)
}
