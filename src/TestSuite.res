open Belt

let filter = _ => true

let summary =
  await [OptionExTests.tests, ResultExTests.allTests]->Array.concatMany->TestEx.runSuite(~filter)

if summary.fail > 0 {
  Js.Exn.raiseError(`Failed tests: ${summary.fail->Int.toString}`)
}
