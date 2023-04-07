open Belt
module Test = Extras__Test

let filter = _ => true

let summary =
  await [
    Extras__CmpTests.tests,
    Extras__ArrayTests.tests,
    Extras__OptionTests.tests,
    Extras__ResultTests.allTests,
    Extras__TaskTest.tests,
    Extras__TaskResultTests.tests,
    Extras__NonEmptyArrayTests.tests,
    Extras__LiteralTests.tests,
    Extras__UnionTests.tests,
  ]
  ->Array.concatMany
  ->Extras__Test.runSuite(~filter)

if summary.fail > 0 {
  Js.Exn.raiseError(`Failed tests: ${summary.fail->Int.toString}`)
}
