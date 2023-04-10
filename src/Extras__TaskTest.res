module T = Extras__Test
module Task = Extras__Task
module Promise = Js.Promise2
module ResultEx = Extras__Result

exception OutOfRange(int)

@get external errorMessage: 'a => option<string> = "message"

let taskTests = {
  let makeTest = (~title, ~expectation, ~a, ~b) =>
    T.makeAsync(~category="Task", ~title, ~expectation, ~predicate=async () =>
      await a()->Task.toPromise == b
    )
  [
    makeTest(
      ~title="make",
      ~expectation="when succeeds, return value",
      ~a=() => Task.make(~promise=() => Promise.resolve(11), ~onError=_ => 3),
      ~b=11,
    ),
    makeTest(
      ~title="make",
      ~expectation="when fails with ReScript exception, return value after onError processes exn",
      ~a=() =>
        Task.make(
          ~promise=() => Promise.resolve(888)->Promise.then(_ => raise(OutOfRange(4))),
          ~onError=e =>
            switch e {
            | OutOfRange(n) => n * 2
            | _ => -1
            },
        ),
      ~b=8,
    ),
    makeTest(
      ~title="make",
      ~expectation="when fails with JavaScript exception, return value after onError processes exn",
      ~a=() =>
        Task.make(
          ~promise=() => Promise.resolve(-76)->Promise.then(_ => Js.Exn.raiseError("failure!")),
          ~onError=e =>
            switch e->errorMessage {
            | Some("failure!") => 99
            | _ => -1
            },
        ),
      ~b=99,
    ),
    makeTest(
      ~title="map",
      ~expectation="return value mapped",
      ~a=() =>
        Task.make(~promise=() => Promise.resolve(2), ~onError=_ => -99)
        ->Task.map(i => i * 2)
        ->Task.map(i => i * 3)
        ->Task.map(i => `It is ${i->Belt.Int.toString}`),
      ~b="It is 12",
    ),
    T.makeAsync(
      ~category="Task",
      ~title="spy",
      ~expectation="spy function is executed",
      ~predicate=async () => {
        let saw = ref(-1)
        let t =
          Task.make(~promise=() => Promise.resolve(99), ~onError=_ => -1)
          ->Task.spy(v => saw := v)
          ->Task.map(i => i * 2)
        let _ignore = await t->Task.toPromise
        saw.contents == 99
      },
    ),
  ]
}

let taskResultTests = {
  let makeTest = (~title, ~expectation, ~a, ~b) =>
    T.makeAsync(~category="Task.Result", ~title, ~expectation, ~predicate=async () =>
      await a()->Task.toPromise == b
    )
  [
    makeTest(
      ~title="make",
      ~expectation="when succeeds, return Ok",
      ~a=() => Task.Result.make(~promise=() => Promise.resolve(11), ~onError=_ => "abc"),
      ~b=Ok(11),
    ),
    makeTest(
      ~title="make",
      ~expectation="when fails with ReScript exception, return Error with onError processing exn",
      ~a=() =>
        Task.Result.make(
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
      ~expectation="when fails with ReScript exception, can map error later",
      ~a=() =>
        Task.Result.make(
          ~promise=() => Promise.resolve("abc")->Promise.then(_ => raise(OutOfRange(4))),
          ~onError=e => e,
        )->Task.Result.mapError(e =>
          switch e {
          | OutOfRange(n) => n * 2
          | _ => -1
          }
        ),
      ~b=Error(8),
    ),
    makeTest(
      ~title="make",
      ~expectation="when fails with JavaScript exception, return Error with onError processing exn",
      ~a=() =>
        Task.Result.make(
          ~promise=() => Promise.resolve("abc")->Promise.then(_ => Js.Exn.raiseError("failure!")),
          ~onError=e =>
            switch e->errorMessage {
            | Some("failure!") => 99
            | _ => -1
            },
        ),
      ~b=Error(99),
    ),
    makeTest(
      ~title="make",
      ~expectation="when fails with JavaScript exception, can map error later",
      ~a=() =>
        Task.Result.make(
          ~promise=() => Promise.resolve("abc")->Promise.then(_ => Js.Exn.raiseError("failure!")),
          ~onError=e => e,
        )->Task.Result.mapError(e =>
          switch e->errorMessage {
          | Some("failure!") => 99
          | _ => -1
          }
        ),
      ~b=Error(99),
    ),
    makeTest(
      ~title="mapOk",
      ~expectation="when succeeds, return Ok mapped",
      ~a=() =>
        Task.Result.make(~promise=() => Promise.resolve(11), ~onError=_ => "abc")
        ->Task.Result.mapOk(i => i * 2)
        ->Task.Result.mapOk(i => i * 5),
      ~b=Ok(11 * 2 * 5),
    ),
    makeTest(
      ~title="mapError",
      ~expectation="when fails, return Error mapped",
      ~a=() =>
        Task.Result.make(
          ~promise=() => Promise.resolve(11)->Promise.then(_ => raise(OutOfRange(4))),
          ~onError=_ => 3,
        )
        ->Task.Result.mapError(i => i * 2)
        ->Task.Result.mapError(i => i + 9),
      ~b=Error(3 * 2 + 9),
    ),
    makeTest(
      ~title="flatMap",
      ~expectation="when succeeds, return Ok",
      ~a=() =>
        Task.Result.make(~promise=() => Promise.resolve(11), ~onError=_ => 3)
        ->Task.Result.flatMap(i => i == 11 ? Ok(88) : Error(-1))
        ->Task.Result.flatMap(i => i == 88 ? Ok(65) : Error(-1)),
      ~b=Ok(65),
    ),
    makeTest(
      ~title="flatMap",
      ~expectation="when fails eventually, return first Error",
      ~a=() =>
        Task.Result.make(~promise=() => Promise.resolve(11), ~onError=_ => 3)
        ->Task.Result.flatMap(i => i == 11 ? Ok(88) : Error(-1))
        ->Task.Result.flatMap(i => i == 88 ? Error(1234) : Ok(-1))
        ->Task.Result.flatMap(_ => Error(55)),
      ~b=Error(1234),
    ),
    makeTest(
      ~title="flatMap",
      ~expectation="when fails immediately, return first Error",
      ~a=() =>
        Task.Result.make(
          ~promise=() => Promise.resolve(11)->Promise.then(_ => raise(OutOfRange(4))),
          ~onError=_ => 103,
        )
        ->Task.Result.flatMap(_ => Error(-1))
        ->Task.Result.flatMap(_ => Error(31)),
      ~b=Error(103),
    ),
    makeTest(
      ~title="toOption",
      ~expectation="when succeeds, return Some",
      ~a=() =>
        Task.Result.make(~promise=() => Promise.resolve(11), ~onError=_ => 3)->Task.Result.toOption,
      ~b=Some(11),
    ),
    makeTest(
      ~title="toOption",
      ~expectation="when fails, return None",
      ~a=() =>
        Task.Result.make(
          ~promise=() => Promise.resolve(11)->Promise.then(_ => raise(OutOfRange(4))),
          ~onError=_ => 3,
        )->Task.Result.toOption,
      ~b=None,
    ),
  ]
}

let tests = Js.Array2.concat(taskTests, taskResultTests)
