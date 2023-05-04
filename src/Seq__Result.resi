let fromArrayMap: (Belt.Array.t<'a>, 'a => result<'b, 'c>) => result<array<'b>, 'c>
let fromArray: Belt.Array.t<result<'a, 'b>> => result<array<'a>, 'b>
let mapError: (result<'a, 'b>, 'b => 'c) => result<'a, 'c>
let fromTryCatch: (unit => 'a) => result<'a, exn>
let toOption: result<'a, 'b> => option<'a>
let getError: result<'a, 'b> => option<'b>
