module AX = Extras__Array
module O = Belt.Option
module Cmp = Extras__Cmp

@unboxed
type t<'a> = NonEmptyArray(array<'a>)

@inline let unwrap = (NonEmptyArray(xs)) => xs

let fromArray = xs =>
  switch xs->AX.isEmpty {
  | true => None
  | false => Some(NonEmptyArray(xs))
  }

external fromArrayUnsafe: array<'a> => t<'a> = "%identity"

let fromArrayExn = xs => xs->fromArray->O.getExn

external toArray: t<'a> => array<'a> = "%identity"

let map = (xs, f) => xs->unwrap->Js.Array2.map(f)->NonEmptyArray
let mapi = (xs, f) => xs->unwrap->Js.Array2.mapi(f)->NonEmptyArray

let head = xs => xs->unwrap->Js.Array2.unsafe_get(0)
let last = xs => xs->unwrap->AX.last->O.getUnsafe

let reduce = (xs, f) =>
  xs->unwrap->Js.Array2.reducei((sum, val, inx) => inx == 0 ? sum : f(sum, val), xs->head)

let minBy = (xs, cmp) => xs->reduce((i, j) => Cmp.min(cmp, i, j))
let maxBy = (xs, cmp) => xs->reduce((i, j) => Cmp.max(cmp, i, j))
