type t

let make: (~category: string, ~title: string, ~expectation: string, ~predicate: unit => bool) => t

let makeAsync: (
  ~category: string,
  ~title: string,
  ~expectation: string,
  ~predicate: unit => promise<bool>,
) => t

let runSuite: (~filter: t => bool=?, array<t>) => promise<unit>
