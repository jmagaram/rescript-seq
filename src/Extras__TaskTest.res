module T = Extras__Test
module Task = Extras__Task
module Promise = Js.Promise2

exception OutOfRange(int)

@get external errorMessage: 'a => option<string> = "message"

let makeTest = (~title, ~expectation, ~a, ~b) =>
  T.makeAsync(~category="Task", ~title, ~expectation, ~predicate=async () =>
    await a()->Task.toPromise == b
  )

let tests = [
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
]
