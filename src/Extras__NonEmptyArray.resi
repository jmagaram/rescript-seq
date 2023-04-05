module Cmp = Extras__Cmp

type t<'a>

let fromArray: array<'a> => option<t<'a>>
external fromArrayUnsafe: array<'a> => t<'a> = "%identity"
let fromArrayExn: array<'a> => t<'a>
external toArray: t<'a> => array<'a> = "%identity"
let map: (t<'a>, 'a => 'b) => t<'b>
let mapi: (t<'a>, ('a, int) => 'b) => t<'b>
let head: t<'a> => 'a
let last: t<'a> => 'a
let reduce: (t<'a>, ('a, 'a) => 'a) => 'a
let maxBy: (t<'a>, Cmp.t<'a>) => 'a
let minBy: (t<'a>, Cmp.t<'a>) => 'a
