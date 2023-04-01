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
      let lazyPromise = () => Promise.resolve("elephant")
      let result = await lazyPromise->Task.make->Task.toPromise
      result == Ok("elephant")
    },
  ),
  T.makeAsync(
    ~category="Task",
    ~title="map",
    ~expectation="when promise succeeds => Ok with mapping",
    ~predicate=async () => {
      let lazyPromise = () => Promise.resolve(2)
      let result =
        await lazyPromise
        ->Task.make
        ->Task.map(i => i * 2)
        ->Task.map(i => i * 3)
        ->Task.map(i => `It is ${i->Belt.Int.toString}`)
        ->Task.toPromise
      result == Ok("It is 12")
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
      let result = await lazyPromise->Task.make->Task.toPromise
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
      let result =
        await lazyPromise
        ->Task.make
        ->Task.mapError(_ => 2)
        ->Task.mapError(i => i * 3)
        ->Task.toPromise
      result == Error(6)
    },
  ),
  // T.makeAsync(
  //   ~category="Task",
  //   ~title="mapError",
  //   ~expectation="when promise fails, receive original error to map",
  //   ~predicate=async () => {
  //     let lazyPromise = async () => {
  //       Js.Exn.raiseError("failed")->ignore
  //       await Promise.resolve(true)
  //     }
  //     let result =
  //       await lazyPromise
  //       ->Task.make
  //       ->Task.mapError(i => i->Js.Exn.asJsExn->Option.flatMap(e => e->Js.Exn.message))
  //       ->Task.toPromise
  //     Js.Console.log(result)
  // result == Error(Some("elephant"))
  // },
  // ),
]
