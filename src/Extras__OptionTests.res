let expectEq = (~title, ~expectation, ~a, ~b) =>
  Extras__Test.make(~category="Option", ~title, ~expectation, ~predicate=() => a() == b)

module T = Extras__Test
module O = Extras__Option

let add = (a, b) => a + b

let tests = [
  expectEq(
    ~title="isSomeAnd",
    ~expectation="when None => false",
    ~a=() => None->O.isSomeAnd(_ => true),
    ~b=false,
  ),
  expectEq(
    ~title="isSomeAnd",
    ~expectation="when Some and condition met => true",
    ~a=() => Some(3)->O.isSomeAnd(i => i == 3),
    ~b=true,
  ),
  expectEq(
    ~title="isSomeAnd",
    ~expectation="when Some and condition not met => false",
    ~a=() => Some(3)->O.isSomeAnd(i => i == 9),
    ~b=false,
  ),
  expectEq(
    ~title="isNoneOr",
    ~expectation="when None => true",
    ~a=() => None->O.isNoneOr(_ => false),
    ~b=true,
  ),
  expectEq(
    ~title="isNoneOr",
    ~expectation="when Some and condition met => true",
    ~a=() => Some(3)->O.isNoneOr(i => i == 3),
    ~b=true,
  ),
  expectEq(
    ~title="isNoneOr",
    ~expectation="when Some and condition not met => false",
    ~a=() => Some(3)->O.isNoneOr(i => i == 9999),
    ~b=false,
  ),
  expectEq(
    ~title="concat",
    ~expectation="when both None => None",
    ~a=() => O.concat(None, None, add),
    ~b=None,
  ),
  expectEq(
    ~title="concat",
    ~expectation="when first is Some => Some",
    ~a=() => O.concat(Some(4), None, add),
    ~b=Some(4),
  ),
  expectEq(
    ~title="concat",
    ~expectation="when second is Some => Some",
    ~a=() => O.concat(None, Some(4), add),
    ~b=Some(4),
  ),
  expectEq(
    ~title="concat",
    ~expectation="when both Some => apply concat function",
    ~a=() => O.concat(Some(3), Some(4), add),
    ~b=Some(7),
  ),
  expectEq(
    ~title="fold",
    ~expectation="when None, return current sum",
    ~a=() => None->O.fold(add, 4),
    ~b=4,
  ),
  expectEq(
    ~title="fold",
    ~expectation="when Some, fold item into sum",
    ~a=() => Some(3)->O.fold(add, 4),
    ~b=7,
  ),
  expectEq(
    ~title="foldBack",
    ~expectation="when None, return current sum",
    ~a=() => 4->O.foldBack(add, None),
    ~b=4,
  ),
  expectEq(
    ~title="foldBack",
    ~expectation="when Some, fold item into sum",
    ~a=() => 3->O.foldBack(add, Some(4)),
    ~b=7,
  ),
  expectEq(~title="toArray", ~expectation="when None => empty", ~a=() => None->O.toArray, ~b=[]),
  expectEq(
    ~title="toArray",
    ~expectation="when Some => array of one item",
    ~a=() => Some(4)->O.toArray,
    ~b=[4],
  ),
  expectEq(~title="fromOk", ~expectation="when Ok => Some", ~a=() => Ok(5)->O.fromOk, ~b=Some(5)),
  expectEq(
    ~title="fromOk",
    ~expectation="when Error => None",
    ~a=() => Error(5)->O.fromOk,
    ~b=None,
  ),
  expectEq(
    ~title="fromError",
    ~expectation="when Error => Some",
    ~a=() => Error(5)->O.fromError,
    ~b=Some(5),
  ),
  expectEq(
    ~title="fromError",
    ~expectation="when Ok => None",
    ~a=() => Ok(5)->O.fromError,
    ~b=None,
  ),
  T.make(
    ~category="Option",
    ~title="fromTryCatch",
    ~expectation="when throw, return as None",
    ~predicate=() => {
      let r = O.fromTryCatch(() => Js.Exn.raiseError("banana"))
      r->Belt.Option.isNone
    },
  ),
  T.make(
    ~category="Option",
    ~title="fromTryCatch",
    ~expectation="when not throw, return result as Some",
    ~predicate=() => O.fromTryCatch(() => 3) == Some(3),
  ),
  T.make(~category="Option", ~title="map2", ~expectation="when both None => None", ~predicate=() =>
    O.map2(None, None, add) == None
  ),
  T.make(
    ~category="Option",
    ~title="map2",
    ~expectation="when both Some => Some with map args",
    ~predicate=() => O.map2(Some(1), Some(5), add) == Some(6),
  ),
  T.make(
    ~category="Option",
    ~title="map2",
    ~expectation="when one is Some => None",
    ~predicate=() => O.map2(Some(1), None, add) == None,
  ),
  T.make(
    ~category="Option",
    ~title="map2",
    ~expectation="when one is Some => None",
    ~predicate=() => O.map2(None, Some(1), add) == None,
  ),
]
