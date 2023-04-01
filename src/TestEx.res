open Belt

type t = {
  category: string,
  title: string,
  expectation: string,
  predicate: unit => promise<bool>,
}

let stringCmp = (x: string, y: string) => x < y ? -1 : x > y ? 1 : 0
let cmp = (a, b) => stringCmp(a.category ++ a.title, b.category ++ b.title)

let make = (~category, ~title, ~expectation, ~predicate as p) => {
  category,
  title,
  expectation,
  predicate: () =>
    try {
      p()->Js.Promise2.resolve
    } catch {
    | _ => false->Js.Promise2.resolve
    },
}

let makeAsync = (~category, ~title, ~expectation, ~predicate) => {
  category,
  title,
  expectation,
  predicate,
}

let run = async i => {
  try {
    let succeed = await i.predicate()
    (succeed, i)
  } catch {
  | _ => (false, i)
  }
}

let toString = i => `${i.category} | ${i.title} | ${i.expectation}`

let runSuite = async (~filter=_ => true, tests) => {
  let log = Js.Console.log
  let logSection = n => {
    log("")
    log(`==== ${n} ====`)
  }
  let results =
    await tests
    ->Array.keep(filter)
    ->SortArray.stableSortBy(cmp)
    ->Array.map(i => i->run)
    ->Js.Promise2.all
  let (succeeded, failed) = results->Array.partition(((r, _)) => r)
  if succeeded->Array.length > 0 {
    logSection("SUCCEEDED")
    succeeded->Array.forEach(((_, t)) => `PASS ${t->toString}`->log)
  }
  if failed->Array.length > 0 {
    logSection("FAILED")
    failed->Array.forEach(((_, t)) => `FAIL  ${t->toString}`->log)
  }
  logSection("SUMMARY")
  log(`PASS : ${succeeded->Array.length->Int.toString}`)
  log(`FAIL : ${failed->Array.length->Int.toString}`)
  log(`RAN  : ${results->Array.length->Int.toString}`)
  log("")
}
