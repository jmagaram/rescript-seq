module T = Extras__Test
module U = Extras__Unknown
module Option = Belt.Option

let tests = [
  T.make(
    ~category="Unknown",
    ~title="isNullOrUndefined",
    ~expectation="when null => true",
    ~predicate=() => Js.null->U.isNullOrUndefined == true,
  ),
  T.make(
    ~category="Unknown",
    ~title="isNullOrUndefined",
    ~expectation="when undefined => true",
    ~predicate=() => Js.undefined->U.isNullOrUndefined == true,
  ),
  T.make(
    ~category="Unknown",
    ~title="isNullOrUndefined",
    ~expectation="when a number => false",
    ~predicate=() => 43->U.isNullOrUndefined == false,
  ),
  T.make(
    ~category="Unknown",
    ~title="isNullOrUndefined",
    ~expectation="when some letters => false",
    ~predicate=() => "abc"->U.isNullOrUndefined == false,
  ),
  T.make(
    ~category="Unknown",
    ~title="getBool",
    ~expectation="when property exists => Some",
    ~predicate=() => {"a": true}->U.getBool("a") == Some(true),
  ),
  T.make(
    ~category="Unknown",
    ~title="getBool",
    ~expectation="when property exists => Some",
    ~predicate=() => {"a": false}->U.getBool("a") == Some(false),
  ),
  T.make(
    ~category="Unknown",
    ~title="getBool",
    ~expectation="when object is null => None",
    ~predicate=() => Js.null->U.getBool("a")->Option.isNone,
  ),
]
