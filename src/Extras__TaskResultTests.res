module T = Extras__Test
module Task = Extras__Task
module TaskResult = Extras__TaskResult
module Promise = Js.Promise2

exception OutOfRange(int)

@get external message: 'a => option<string> = "message"

let makeTest = (~title, ~expectation, ~a, ~b) =>
  T.makeAsync(~category="TaskResult", ~title, ~expectation, ~predicate=async () =>
    await a()->TaskResult.toPromise == b
  )

let tests = [
  makeTest(
    ~title="make",
    ~expectation="when succeeds, return Ok",
    ~a=() => TaskResult.make(~promise=() => Promise.resolve(11), ~onError=_ => "abc"),
    ~b=Ok(11),
  ),
  makeTest(
    ~title="make",
    ~expectation="when fails with ReScript exception, return Error with onError processing exn",
    ~a=() =>
      TaskResult.make(
        ~promise=() => Promise.resolve("abc")->Promise.then(_ => raise(OutOfRange(4))),
        ~onError=e =>
          switch e {
          | OutOfRange(n) => n * 2
          | _ => -1
          },
      ),
    ~b=Error(8),
  ),
  makeTest(
    ~title="make",
    ~expectation="when fails with JavaScript exception, return Error with onError processing exn",
    ~a=() =>
      TaskResult.make(
        ~promise=() => Promise.resolve("abc")->Promise.then(_ => Js.Exn.raiseError("failure!")),
        ~onError=e =>
          switch e->message {
          | Some("failure!") => 99
          | _ => -1
          },
      ),
    ~b=Error(99),
  ),
  makeTest(
    ~title="mapOk",
    ~expectation="when succeeds, return Ok mapped",
    ~a=() =>
      TaskResult.make(~promise=() => Promise.resolve(11), ~onError=_ => "abc")
      ->TaskResult.mapOk(i => i * 2)
      ->TaskResult.mapOk(i => i * 5),
    ~b=Ok(11 * 2 * 5),
  ),
  makeTest(
    ~title="mapError",
    ~expectation="when fails, return Error mapped",
    ~a=() =>
      TaskResult.make(
        ~promise=() => Promise.resolve(11)->Promise.then(_ => raise(OutOfRange(4))),
        ~onError=_ => 3,
      )
      ->TaskResult.mapError(i => i * 2)
      ->TaskResult.mapError(i => i + 9),
    ~b=Error(3 * 2 + 9),
  ),
  makeTest(
    ~title="flatMap",
    ~expectation="when succeeds, return Ok",
    ~a=() =>
      TaskResult.make(~promise=() => Promise.resolve(11), ~onError=_ => 3)
      ->TaskResult.flatMap(i => i == 11 ? Ok(88) : Error(-1))
      ->TaskResult.flatMap(i => i == 88 ? Ok(65) : Error(-1)),
    ~b=Ok(65),
  ),
  makeTest(
    ~title="flatMap",
    ~expectation="when fails eventually, return first Error",
    ~a=() =>
      TaskResult.make(~promise=() => Promise.resolve(11), ~onError=_ => 3)
      ->TaskResult.flatMap(i => i == 11 ? Ok(88) : Error(-1))
      ->TaskResult.flatMap(i => i == 88 ? Error(1234) : Ok(-1))
      ->TaskResult.flatMap(_ => Error(55)),
    ~b=Error(1234),
  ),
  makeTest(
    ~title="flatMap",
    ~expectation="when fails immediately, return first Error",
    ~a=() =>
      TaskResult.make(
        ~promise=() => Promise.resolve(11)->Promise.then(_ => raise(OutOfRange(4))),
        ~onError=_ => 103,
      )
      ->TaskResult.flatMap(_ => Error(-1))
      ->TaskResult.flatMap(_ => Error(31)),
    ~b=Error(103),
  ),
]
