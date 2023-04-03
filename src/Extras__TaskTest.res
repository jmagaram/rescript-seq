module T = Extras__Test
module Task = Extras__Task
module Promise = Js.Promise2
module Option = Belt.Option

exception OutOfRange(int)

@get external message: 'a => option<string> = "message"

let makeTest = (~title, ~expectation, ~a, ~b) =>
  T.makeAsync(~category="Task", ~title, ~expectation, ~predicate=async () =>
    await a()->Task.toPromise == b
  )

let tests = [
  makeTest(
    ~title="make",
    ~expectation="when promise succeeds, return Ok",
    ~a=() => Task.make(~promise=() => Promise.resolve(11), ~onError=_ => 3),
    ~b=Ok(11),
  ),
  makeTest(
    ~title="make",
    ~expectation="when promise fails with ReScript exception, return Error with onError processing exn",
    ~a=() =>
      Task.make(
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
    ~expectation="when promise fails with JavaScript exception, return Error with onError processing exn",
    ~a=() =>
      Task.make(
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
    ~title="makeInfallible",
    ~expectation="when promise succeeds, return value",
    ~a=() => Task.makeInfallible(~promise=() => Promise.resolve(11), ~onError=_ => 3),
    ~b=11,
  ),
  makeTest(
    ~title="makeInfallible",
    ~expectation="when promise fails with ReScript exception, return value after onError processes exn",
    ~a=() =>
      Task.makeInfallible(
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
    ~title="makeInfallible",
    ~expectation="when promise fails with JavaScript exception, return value after onError processes exn",
    ~a=() =>
      Task.makeInfallible(
        ~promise=() => Promise.resolve(-76)->Promise.then(_ => Js.Exn.raiseError("failure!")),
        ~onError=e =>
          switch e->message {
          | Some("failure!") => 99
          | _ => -1
          },
      ),
    ~b=99,
  ),
  makeTest(
    ~title="map",
    ~expectation="when promise succeeds, return value mapped",
    ~a=() =>
      Task.makeInfallible(~promise=() => Promise.resolve(2), ~onError=_ => -99)
      ->Task.map(i => i * 2)
      ->Task.map(i => i * 3)
      ->Task.map(i => `It is ${i->Belt.Int.toString}`),
    ~b="It is 12",
  ),
  makeTest(
    ~title="mapOk",
    ~expectation="when promise succeeds, return Ok mapped",
    ~a=() =>
      Task.make(~promise=() => Promise.resolve(11), ~onError=_ => "abc")->Task.mapOk(i => i * 2),
    ~b=Ok(22),
  ),
  makeTest(
    ~title="mapError",
    ~expectation="when promise fails, return Error mapped",
    ~a=() =>
      Task.make(
        ~promise=() => Promise.resolve(11)->Promise.then(_ => raise(OutOfRange(4))),
        ~onError=_ => 3,
      )
      ->Task.mapError(i => i * 2)
      ->Task.mapError(i => i + 9),
    ~b=Error(15),
  ),
]
