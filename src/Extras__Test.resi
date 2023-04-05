@genType
type t

let make: (~category: string, ~title: string, ~expectation: string, ~predicate: unit => bool) => t

let category: t => string
let title: t => string
let expectation: t => string

let makeAsync: (
  ~category: string,
  ~title: string,
  ~expectation: string,
  ~predicate: unit => promise<bool>,
) => t

type summary = {
  pass: int,
  fail: int,
  ran: int,
}

let runSuite: (~filter: t => bool=?, array<t>) => promise<summary>
