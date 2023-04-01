let expectEqual = (~title, ~expectation, ~a, ~b) =>
  TestEx.make(~category="Option", ~title, ~expectation, ~predicate=() => a() == b)

let tests = [
  expectEqual(
    ~title="isSomeAnd",
    ~expectation="when None => false",
    ~a=() => None->OptionEx.isSomeAnd(_ => true),
    ~b=false,
  ),
  expectEqual(
    ~title="isSomeAnd",
    ~expectation="when Some and condition met => true",
    ~a=() => Some(3)->OptionEx.isSomeAnd(i => i == 3),
    ~b=true,
  ),
  expectEqual(
    ~title="isSomeAnd",
    ~expectation="when Some and condition not met => false",
    ~a=() => Some(3)->OptionEx.isSomeAnd(i => i == 9),
    ~b=false,
  ),
  expectEqual(
    ~title="isNoneOr",
    ~expectation="when None => true",
    ~a=() => None->OptionEx.isNoneOr(_ => false),
    ~b=true,
  ),
  expectEqual(
    ~title="isNoneOr",
    ~expectation="when Some and condition met => true",
    ~a=() => Some(3)->OptionEx.isNoneOr(i => i == 3),
    ~b=true,
  ),
  expectEqual(
    ~title="isNoneOr",
    ~expectation="when Some and condition not met => false",
    ~a=() => Some(3)->OptionEx.isNoneOr(i => i == 9999999999),
    ~b=true,
  ),
]
