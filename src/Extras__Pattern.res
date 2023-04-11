module Option = Belt_Option
module Unknown = Extras__Unknown

module type T = {
  type t
  let isTypeOf: unknown => bool
  let equals: (t, t) => bool
}

module MakeTools = (P: T) => {
  let make = x => x->Unknown.make->P.isTypeOf ? Some((Obj.magic(x): P.t)) : None
  let eq = (x, y) => x->make->Option.flatMap(x => y->make->Option.map(y => P.equals(x, y)))
}

module Int = {
  type t = int
  let isTypeOf = u => u->Unknown.typeof == #number
  let equals = (x: int, y: int) => x === y
}

module Float = {
  type t = float
  let isTypeOf = u => u->Unknown.typeof == #number
  let equals = (x: float, y: float) => x === y
}

module Bool = {
  type t = bool
  let isTypeOf = u => u->Unknown.typeof == #boolean
  let equals = (x: bool, y: bool) => x === y
}

module String = {
  type t = string
  let isTypeOf = u => u->Unknown.typeof == #string
  let equals = (x: string, y: string) => Js.String2.localeCompare(x, y) == 0.0
}

// https://stackoverflow.com/questions/643782/how-to-check-whether-an-object-is-a-date
module Date = {
  type t = Js.Date.t
  let isTypeOf: 'a => bool = %raw(`function (a) { return (!isNaN(a) && (a instanceof Date) && (typeof a.getMonth === 'function')) }`)
  let equals = (x: Js.Date.t, y: Js.Date.t) => x == y
}
