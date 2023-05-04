module Cmp = Extras__Cmp

type t<'a>

let fromArray: array<'a> => option<t<'a>>
let fromArrayExn: array<'a> => t<'a>
external fromArrayUnsafe: array<'a> => t<'a> = "%identity"

@val external of1: 'a => t<'a> = "Array.of"
@val external of2: ('a, 'a) => t<'a> = "Array.of"
@val external of3: ('a, 'a, 'a) => t<'a> = "Array.of"
@variadic @val external ofMany: ('a, array<'a>) => t<'a> = "Array.of"

external toArray: t<'a> => array<'a> = "%identity"

let map: (t<'a>, 'a => 'b) => t<'b>
let mapi: (t<'a>, ('a, int) => 'b) => t<'b>
let head: t<'a> => 'a
let last: t<'a> => 'a
let reduce: (t<'a>, ('a, 'a) => 'a) => 'a
let maxBy: (t<'a>, Cmp.t<'a>) => 'a
let minBy: (t<'a>, Cmp.t<'a>) => 'a
let concat: (t<'a>, t<'a>) => t<'a>
let concatArray: (t<'a>, array<'a>) => t<'a>
