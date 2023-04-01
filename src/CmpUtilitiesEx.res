module Cmp = CmpEx

module type Equality = {
  type t
  let eq: (t, t) => bool
  let neq: (t, t) => bool
}

module type Ordering = {
  type t
  let lt: (t, t) => bool
  let lte: (t, t) => bool
  let gt: (t, t) => bool
  let gte: (t, t) => bool
  let min: (t, t) => t
  let max: (t, t) => t
}

module MakeEquals = (
  C: {
    type domain
    let cmp: CmpEx.t<domain>
  },
): (Equality with type t := C.domain) => {
  let eq = C.cmp->CmpEx.eq
  let neq = C.cmp->Cmp.neq
}

module MakeCompare = (
  C: {
    type domain
    let cmp: Cmp.t<domain>
  },
): (Ordering with type t := C.domain) => {
  let lt = C.cmp->Cmp.lt
  let lte = C.cmp->Cmp.lte
  let gt = C.cmp->Cmp.gt
  let gte = C.cmp->Cmp.gte
  let min = C.cmp->Cmp.min
  let max = C.cmp->Cmp.max
}
