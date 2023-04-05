@genType
type t<'a> = ('a, 'a) => int

let eval = (t, x, y) => t(x, y)
let fromFloatResult = (cmp, x, y) => cmp(x, y)->Belt.Int.fromFloat
let fromMap = (f, t, x, y) => eval(t, f(x), f(y))

let reverse = (t, x, y) => eval(t, y, x)

let eq = (t, x, y) => eval(t, x, y) === 0
let neq = (t, x, y) => eval(t, x, y) !== 0

let lt = (t, x, y) => eval(t, x, y) < 0
let lte = (t, x, y) => eval(t, x, y) <= 0
let gt = (t, x, y) => eval(t, x, y) > 0
let gte = (t, x, y) => eval(t, x, y) >= 0

let min = (t, x, y) => lte(t, x, y) ? x : y
let max = (t, x, y) => lte(t, x, y) ? y : x

let int: t<int> = (x: int, y: int) => x < y ? -1 : x > y ? 1 : 0
let float: t<float> = (x: float, y: float) => x < y ? -1 : x > y ? 1 : 0
let bool: t<bool> = (x: bool, y: bool) => x < y ? -1 : x > y ? 1 : 0
let string = Js.String2.localeCompare->fromFloatResult
