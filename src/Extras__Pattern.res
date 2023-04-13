module Option = Belt_Option
module Unknown = Extras__Unknown

module type T = {
  type t
  let isTypeOf: unknown => bool
  let equals: (t, t) => bool
}

module MakeOption = (P: T) => {
  type t = option<P.t>
  let isTypeOf = v =>
    switch v->Unknown.isUndefined {
    | true => true
    | false => P.isTypeOf(v)
    }
  let equals = (a, b) => Option.eq(a, b, P.equals)
}

module MakeNullable = (P: T) => {
  type t = Js.Nullable.t<P.t>
  let isTypeOf = (v: unknown) =>
    switch v->Unknown.isNullOrUndefined {
    | true => true
    | false => P.isTypeOf(v->Unknown.make)
    }
  let equals = (a: t, b: t) => {
    switch Unknown.isNull(a) {
    | true => Unknown.isNull(b)
    | false =>
      switch Unknown.isUndefined(a) {
      | true => Unknown.isUndefined(b)
      | false => !Unknown.isNullOrUndefined(b) && P.equals(a->Obj.magic, b->Obj.magic)
      }
    }
  }
}

module MakeNull = (P: T) => {
  type t = Js.Null.t<P.t>
  let isTypeOf = (v: unknown) =>
    switch v->Unknown.isNull {
    | true => true
    | false => P.isTypeOf(v->Unknown.make)
    }
  let equals = (a: t, b: t) =>
    switch a->Unknown.isNull {
    | true => b->Unknown.isNull
    | false => !(b->Unknown.isNull) && P.equals(a->Obj.magic, b->Obj.magic)
    }
}

module MakeTuple2 = (
  P: {
    module A: T
    module B: T
  },
) => {
  type t = (P.A.t, P.B.t)
  let isTypeOf = (u: unknown) =>
    u->Js.Array2.isArray &&
    u->Obj.magic->Js.Array2.length == 2 &&
    P.A.isTypeOf(u->Obj.magic->Js.Array2.unsafe_get(0)) &&
    P.B.isTypeOf(u->Obj.magic->Js.Array2.unsafe_get(1))
  let equals = ((a1, b1): t, (a2, b2): t) => P.A.equals(a1, a2) && P.B.equals(b1, b2)
}

module MakeTuple3 = (
  P: {
    module A: T
    module B: T
    module C: T
  },
) => {
  type t = (P.A.t, P.B.t, P.C.t)
  let isTypeOf = (u: unknown) =>
    u->Js.Array2.isArray &&
    u->Obj.magic->Js.Array2.length == 3 &&
    P.A.isTypeOf(u->Obj.magic->Js.Array2.unsafe_get(0)) &&
    P.B.isTypeOf(u->Obj.magic->Js.Array2.unsafe_get(1)) &&
    P.C.isTypeOf(u->Obj.magic->Js.Array2.unsafe_get(2))
  let equals = ((a1, b1, c1): t, (a2, b2, c2): t) =>
    P.A.equals(a1, a2) && P.B.equals(b1, b2) && P.C.equals(c1, c2)
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
