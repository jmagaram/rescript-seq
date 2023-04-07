type unknown
type t = unknown

external toUnknown: 'a => unknown = "%identity"

type typeof = [
  | #undefined
  | #object
  | #boolean
  | #number
  | #bigint
  | #string
  | #symbol
  | #function
]

external typeof: 'a => typeof = "#typeof"

type object
type function
type bigInt = Js.Types.bigint_val
type symbol = Js.Types.symbol

// typeof null == "object". Gotta love that! Do better here.
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof#typeof_null
type jsType =
  | Undefined
  | Null
  | Object(object)
  | Bool(bool)
  | Number(float)
  | BigInt(bigInt)
  | String(string)
  | Symbol(symbol)
  | Function(function)

external toObjectUnsafe: 'a => object = "%identity"
external toBoolUnsafe: 'a => bool = "%identity"
external toFloatUnsafe: 'a => float = "%identity"
external toBigIntUnsafe: 'a => bigInt = "%identity"
external toStringUnsafe: 'a => string = "%identity"
external toSymbolUnsafe: 'a => symbol = "%identity"
external toFunctionUnsafe: 'a => function = "%identity"

let isNull: 'a => bool = %raw(`function (a) { return a === null }`)
let isUndefined: 'a => bool = %raw(`function (a) { return a === undefined }`)
let isNullOrUndefined: 'a => bool = %raw(`function (a) { return a === undefined  || a === undefined }`)

let classify = value => {
  switch typeof(value) {
  | #number => value->toFloatUnsafe->Number
  | #string => value->toStringUnsafe->String
  | #boolean => value->toBoolUnsafe->Bool
  | #undefined => Undefined
  | #function => value->toFunctionUnsafe->Function
  | #bigint => value->toBigIntUnsafe->BigInt
  | #symbol => value->toSymbolUnsafe->Symbol
  | #object =>
    switch isNull(value) {
    | true => Null
    | false => value->toObjectUnsafe->Object
    }
  }
}

let toObject = i => typeof(i) === #object ? Some((Obj.magic(i): object)) : None
let toBool = i => typeof(i) === #boolean ? Some((Obj.magic(i): bool)) : None
let toFloat = i => typeof(i) === #number ? Some((Obj.magic(i): float)) : None
let toBigInt = i => typeof(i) === #bigint ? Some((Obj.magic(i): bigInt)) : None
let toString = i => typeof(i) === #string ? Some((Obj.magic(i): string)) : None
let toSymbol = i => typeof(i) === #symbol ? Some((Obj.magic(i): symbol)) : None
let toFunction = i => typeof(i) === #function ? Some((Obj.magic(i): function)) : None

// Implicitly creates a wrapper object for primitives that is promptly discard.
// Throws if the object is null or undefined.
@get_index external getUnsafe: ('a, string) => unknown = ""
@get_index external getBySymbolUnsafe: ('a, symbol) => unknown = ""

let getObject = (o, n) => isNullOrUndefined(o) ? None : o->getUnsafe(n)->toObject
let getObjectBySymbol = (o, s) => isNullOrUndefined(o) ? None : o->getBySymbolUnsafe(s)->toObject

let getBool = (o, n) => isNullOrUndefined(o) ? None : o->getUnsafe(n)->toBool
let getBoolBySymbol = (o, s) => isNullOrUndefined(o) ? None : o->getBySymbolUnsafe(s)->toBool

let getFloat = (o, n) => isNullOrUndefined(o) ? None : o->getUnsafe(n)->toFloat
let getFloatBySymbol = (o, s) => isNullOrUndefined(o) ? None : o->getBySymbolUnsafe(s)->toFloat

let getBigInt = (o, n) => isNullOrUndefined(o) ? None : o->getUnsafe(n)->toBigInt
let getBigIntBySymbol = (o, s) => isNullOrUndefined(o) ? None : o->getBySymbolUnsafe(s)->toBigInt

let getString = (o, n) => isNullOrUndefined(o) ? None : o->getUnsafe(n)->toString
let getStringBySymbol = (o, s) => isNullOrUndefined(o) ? None : o->getBySymbolUnsafe(s)->toString

let getSymbol = (o, n) => isNullOrUndefined(o) ? None : o->getUnsafe(n)->toSymbol
let getSymbolBySymbol = (o, s) => isNullOrUndefined(o) ? None : o->getBySymbolUnsafe(s)->toSymbol

let getFunction = (o, n) => isNullOrUndefined(o) ? None : o->getUnsafe(n)->toFunction
let getFunctionBySymbol = (o, s) =>
  isNullOrUndefined(o) ? None : o->getBySymbolUnsafe(s)->toFunction
