module T = Extras__Test
module Task = Extras__Task
module Promise = Js.Promise2
module Option = Belt.Option

exception BadInput

let tests = [
  T.makeAsync(
    ~category="Task",
    ~title="make",
    ~expectation="when promise succeeds, return value",
    ~predicate=async () => {
      let result =
        await Task.makeInfallible(
          ~promise=() => Promise.resolve("elephant"),
          ~onError=_ => "banana",
        )->Task.toPromise
      result == "elephant"
    },
  ),
  T.makeAsync(
    ~category="Task",
    ~title="map",
    ~expectation="when promise succeeds, return value mapped",
    ~predicate=async () => {
      let result =
        await Task.makeInfallible(~promise=() => Promise.resolve(2), ~onError=_ => -99)
        ->Task.map(i => i * 2)
        ->Task.map(i => i * 3)
        ->Task.map(i => `It is ${i->Belt.Int.toString}`)
        ->Task.toPromise
      result == "It is 12"
    },
  ),
  T.makeAsync(
    ~category="Task",
    ~title="makeResult",
    ~expectation="when promise succeeds, return Ok",
    ~predicate=async () => {
      let result = await Task.make(() => Promise.resolve(11))->Task.toPromise
      result == Ok(11)
    },
  ),
  T.makeAsync(
    ~category="Task",
    ~title="makeResult",
    ~expectation="when promise fails, return Error",
    ~predicate=async () => {
      let result = await Task.make(() => Promise.reject(BadInput))->Task.toPromise
      result->Belt.Result.isError
    },
  ),
  T.makeAsync(
    ~category="Task",
    ~title="makeResult",
    ~expectation="when promise fails, ensure exn details inside Error",
    ~predicate=async () => {
      let result = await Task.make(() => Promise.reject(BadInput))->Task.toPromise
      result == Error(BadInput)
    },
  ),
]
