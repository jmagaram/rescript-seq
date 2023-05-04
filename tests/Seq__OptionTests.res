let expectEq = (~title, ~expectation, ~a, ~b) =>
  Seq__Test.fromPredicate(~category="Option", ~title, ~expectation, () => a() == b)

module T = Seq__Test
module O = Seq__Option

let add = (a, b) => a + b
let add3 = (a, b, c) => a + b + c
let add4 = (a, b, c, d) => a + b + c + d

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
  T.fromPredicate(
    ~category="Option",
    ~title="fromTryCatch",
    ~expectation="when throw, return as None",
    () => {
      let r = O.fromTryCatch(() => Js.Exn.raiseError("banana"))
      r->Belt.Option.isNone
    },
  ),
  T.fromPredicate(
    ~category="Option",
    ~title="fromTryCatch",
    ~expectation="when not throw, return result as Some",
    () => O.fromTryCatch(() => 3) == Some(3),
  ),
  expectEq(
    ~title="map2",
    ~expectation="when 0/2 Some => None",
    ~a=() => O.map2(None, None, add),
    ~b=None,
  ),
  expectEq(
    ~title="map2",
    ~expectation="when 2/2 Some => Some with map args",
    ~a=() => O.map2(Some(1), Some(5), add),
    ~b=Some(6),
  ),
  expectEq(
    ~title="map2",
    ~expectation="when 1/2 is Some => None",
    ~a=() => O.map2(Some(1), None, add),
    ~b=None,
  ),
  expectEq(
    ~title="map2",
    ~expectation="when 1/2 is Some => None",
    ~a=() => O.map2(None, Some(1), add),
    ~b=None,
  ),
  expectEq(
    ~title="map3",
    ~expectation="when 3/3 is Some => Some with map args",
    ~a=() => O.map3(Some(1), Some(5), Some(9), add3),
    ~b=Some(15),
  ),
  expectEq(
    ~title="map3",
    ~expectation="when 2/3 is Some => None",
    ~a=() => O.map3(Some(1), None, Some(5), add3),
    ~b=None,
  ),
  expectEq(
    ~title="map3",
    ~expectation="when 1/3 is Some => None",
    ~a=() => O.map3(None, Some(1), None, add3),
    ~b=None,
  ),
  expectEq(
    ~title="map3",
    ~expectation="when 0/3 is Some => None",
    ~a=() => O.map3(None, None, None, add3),
    ~b=None,
  ),
  expectEq(
    ~title="orElseWith",
    ~expectation="when first is Some, return it",
    ~a=() => O.orElseWith(Some(1), () => Some(2)),
    ~b=Some(1),
  ),
  expectEq(
    ~title="orElseWith",
    ~expectation="when first is None, return lazy second",
    ~a=() => O.orElseWith(None, () => Some(2)),
    ~b=Some(2),
  ),
  expectEq(
    ~title="flatten",
    ~expectation="when Some Some => Some",
    ~a=() => Some(Some(4))->O.flatten,
    ~b=Some(4),
  ),
  expectEq(
    ~title="flatten",
    ~expectation="when Some None => None",
    ~a=() => Some(None)->O.flatten,
    ~b=None,
  ),
]
