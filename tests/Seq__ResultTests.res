open Belt

module T = Seq__Test
module R = Seq__Result

let expectEqual = (~title, ~expectation, ~a, ~b) =>
  T.fromPredicate(~category="Result", ~title, ~expectation, () => a() == b)

let fromArray =
  [
    ("when empty, return empty", [], Ok([])),
    ("when one error, return it", [Error("a")], Error("a")),
    ("when one ok, return it", [Ok(1)], Ok([1])),
    ("when all ok, return all", [Ok(1), Ok(2), Ok(3)], Ok([1, 2, 3])),
    ("when all error, return first", [Error("a"), Error("b"), Error("c")], Error("a")),
    ("when mix, return first error", [Ok(1), Error("a"), Ok(2), Error("b")], Error("a")),
  ]->Array.map(((expectation, input, expected)) =>
    expectEqual(~title="fromArray", ~expectation, ~a=() => input->R.fromArray, ~b=expected)
  )

let fromArrayMap = {
  let mapper = n => n < 10 ? Ok(n * 2) : Error(n->Int.toString)
  [
    ("when empty, return empty", [], Ok([])),
    ("when one error, return it", [30], Error("30")),
    ("when one ok, return it", [2], Ok([4])),
    ("when all ok, return all", [1, 2, 3], Ok([2, 4, 6])),
    ("when all error, return first", [20, 30, 40], Error("20")),
    ("when mix, return first error", [1, 2, 14, 3, 4], Error("14")),
  ]->Array.map(((expectation, input, expected)) =>
    expectEqual(
      ~title="fromArrayMap",
      ~expectation,
      ~a=() => input->R.fromArrayMap(mapper),
      ~b=expected,
    )
  )
}

let mapError =
  [
    ("when Error, map the error", Error(3), Error(6)),
    ("when Ok, return Ok", Ok("a"), Ok("a")),
  ]->Array.map(((expectation, input, expected)) =>
    expectEqual(
      ~title="mapError",
      ~expectation,
      ~a=() => input->R.mapError(i => i * 2),
      ~b=expected,
    )
  )

let fromTryCatch = [
  T.fromPredicate(
    ~category="Result",
    ~title="fromTryCatch",
    ~expectation="when throw, return as Error",
    () => {
      let r = R.fromTryCatch(() => Js.Exn.raiseError("banana"))
      switch r {
      | Ok(_) => false
      | Error(e) =>
        switch e->Js.Exn.asJsExn {
        | Some(err) => err->Js.Exn.message == Some("banana")
        | _ => false
        }
      }
    },
  ),
  T.fromPredicate(
    ~category="Result",
    ~title="fromTryCatch",
    ~expectation="when not throw, return result as Ok",
    () => R.fromTryCatch(() => 3) == Ok(3),
  ),
]

let toOption = [
  expectEqual(
    ~title="toOption",
    ~expectation="when Ok, return Some",
    ~a=() => Ok(4)->R.toOption,
    ~b=Some(4),
  ),
  expectEqual(
    ~title="toOption",
    ~expectation="when Error, return None",
    ~a=() => Error(4)->R.toOption,
    ~b=None,
  ),
]

let getError = [
  expectEqual(
    ~title="getError",
    ~expectation="when Ok, return None",
    ~a=() => Ok(4)->R.getError,
    ~b=None,
  ),
  expectEqual(
    ~title="getError",
    ~expectation="when Error, return Some",
    ~a=() => Error(4)->R.getError,
    ~b=Some(4),
  ),
]

let allTests =
  [toOption, getError, fromTryCatch, fromArray, fromArrayMap, mapError]->Array.concatMany
