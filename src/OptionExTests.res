let expectEq = (~title, ~expectation, ~a, ~b) =>
  TestEx.make(~category="Option", ~title, ~expectation, ~predicate=() => a() == b)

module O = OptionEx

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
]
