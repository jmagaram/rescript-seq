module T = Extras__Test
module Task = Extras__Task
module Promise = Js.Promise2
module Option = Belt.Option

let tests = [
  T.makeAsync(
    ~category="Task",
    ~title="toResult",
    ~expectation="when promise succeeds => Ok",
    ~predicate=async () => {
      let lazyPromise = () => Promise.resolve(8)
      let result = await lazyPromise->Task.make->Task.map(i => i * 2)->Task.toResult
      result == Ok(16)
    },
  ),
  T.makeAsync(
    ~category="Task",
    ~title="toResult",
    ~expectation="when promise fails => Error",
    ~predicate=async () => {
      let lazyPromise = async () => {
        Js.Exn.raiseError("failed")->ignore
        await Promise.resolve(true)
      }
      let result = await lazyPromise->Task.make->Task.toResult
      result->Belt.Result.isError
    },
  ),
  T.makeAsync(
    ~category="Task",
    ~title="mapError",
    ~expectation="when promise fails => Error with mapping",
    ~predicate=async () => {
      let lazyPromise = async () => {
        Js.Exn.raiseError("failed")->ignore
        await Promise.resolve(true)
      }
      let result = await lazyPromise->Task.make->Task.mapError(_ => "banana")->Task.toResult
      result == Error("banana")
    },
  ),
  T.makeAsync(
    ~category="Task",
    ~title="mapError",
    ~expectation="when promise fails => Error with mapping",
    ~predicate=async () => {
      let lazyPromise = async () => {
        Js.Exn.raiseError("failed")->ignore
        await Promise.resolve(true)
      }
      let result =
        await lazyPromise
        ->Task.make
        ->Task.mapError(_ => 2)
        ->Task.mapError(i => i * 2)
        ->Task.mapError(i => i * 2)
        ->Task.toResult
      result == Error(8)
    },
  ),
]
