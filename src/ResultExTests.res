open Belt

let expectEqual = (~title, ~expectation, ~a, ~b) =>
  TestEx.make(~category="Result", ~title, ~expectation, ~predicate=() => a() == b)

let fromArray =
  [
    ("when empty, return empty", [], Ok([])),
    ("when one error, return it", [Error("a")], Error("a")),
    ("when one ok, return it", [Ok(1)], Ok([1])),
    ("when all ok, return all", [Ok(1), Ok(2), Ok(3)], Ok([1, 2, 3])),
    ("when all error, return first", [Error("a"), Error("b"), Error("c")], Error("a")),
    ("when mix, return first error", [Ok(1), Error("a"), Ok(2), Error("b")], Error("a")),
  ]->Array.map(((expectation, input, expected)) =>
    expectEqual(~title="fromArray", ~expectation, ~a=() => input->ResultEx.fromArray, ~b=expected)
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
      ~a=() => input->ResultEx.fromArrayMap(mapper),
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
      ~a=() => input->ResultEx.mapError(i => i * 2),
      ~b=expected,
    )
  )

let fromTryCatch = [
  TestEx.make(
    ~category="Result",
    ~title="fromTryCatch",
    ~expectation="when throw, return as Error",
    ~predicate=() => {
      let r = ResultEx.fromTryCatch(() => Js.Exn.raiseError("banana"))
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
  TestEx.make(
    ~category="Result",
    ~title="fromTryCatch",
    ~expectation="when not throw, return result as Ok",
    ~predicate=() => ResultEx.fromTryCatch(() => 3) == Ok(3),
  ),
]

let ok = [
  expectEqual(
    ~title="ok",
    ~expectation="when Ok, return Some",
    ~a=() => Ok(4)->ResultEx.ok,
    ~b=Some(4),
  ),
  expectEqual(
    ~title="ok",
    ~expectation="when Error, return None",
    ~a=() => Error(4)->ResultEx.ok,
    ~b=None,
  ),
]

let error = [
  expectEqual(
    ~title="error",
    ~expectation="when Ok, return None",
    ~a=() => Ok(4)->ResultEx.error,
    ~b=None,
  ),
  expectEqual(
    ~title="error",
    ~expectation="when Error, return Some",
    ~a=() => Error(4)->ResultEx.error,
    ~b=Some(4),
  ),
]

let allTests = [ok, error, fromTryCatch, fromArray, fromArrayMap, mapError]->Array.concatMany
