/***
Utilities for classifying and safely using JavaScript values at runtime.
*/

/**
An abstract representation for values whose contents are a mystery, or at least not 100% guaranteed to be what you expect. A value of type `unknown` could be undefined, null, a number, string, function, or any other JavaScript type. To safely use an `unknown` value use the functions in this module to test and classify it as its true underlying type.
*/
type unknown
type t = unknown

/**
The possible types of JavaScript values. See [typeof on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof)
*/
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

/**
`make` convert any type to `unknown`, forcing users of that value to test its contents before using it.
*/
external make: 'a => unknown = "%identity"

/**
`typeof(value)` returns the underlying JavaScript type of `value` at runtime. **Note** null values return `object`. See [typeof on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof)

## Examples
```rescript
Type.typeof("Hello") // "string"
Type.typeof(3) // "number"
Type.typeof(3.14) // "number"
Type.typeof(null) // "object"
```
*/
external typeof: 'a => typeof = "#typeof"

type object
type function
type bigInt = Js.Types.bigint_val
type symbol = Js.Types.symbol

/**
Represents a classified JavaScript value. **Note:** Unlike ReScript, JavaScript does not distinguish between floats and ints. A classified `float` can be later converted to an `int` if desired.
*/
type jsType =
  | Undefined
  | Null
  | Object(object)
  | Bool(bool)
  | Number(float)
  | BigInt(Js.Types.bigint_val)
  | String(string)
  | Symbol(Js.Types.symbol)
  | Function(function)

/**
`classify(value)` inspects a value and determines its type.

## Examples
```rescript
switch %raw(`null`)->classify {
| Null => Console.log("Yup, that's null.")
| _ => Console.log("This doesn't actually appear to be null...")
}
```
*/
let classify: 'a => jsType

let isUndefined: 'a => bool
let isNull: 'a => bool
let isNullOrUndefined: 'a => bool

// Safely convert any type to the desigated type; a convenience wrapper around the `classify` function.
let toObject: 'a => option<object>
let toBool: 'a => option<bool>
let toFloat: 'a => option<float>
let toBigInt: 'a => option<Js.Types.bigint_val>
let toString: 'a => option<string>
let toSymbol: 'a => option<Js.Types.symbol>
let toFunction: 'a => option<function>

// Unsafely converts any type to the designated type.
external toObjectUnsafe: 'a => object = "%identity"
external toBoolUnsafe: 'a => bool = "%identity"
external toFloatUnsafe: 'a => float = "%identity"
external toBigIntUnsafe: 'a => bigInt = "%identity"
external toStringUnsafe: 'a => string = "%identity"
external toSymbolUnsafe: 'a => symbol = "%identity"
external toFunctionUnsafe: 'a => function = "%identity"

// Safely access properties on unknown types either by name or symbol. These
// have safety checks: (a) The object is not null or undefined, and (b) the
// return value must be what the user expects it to be. These functions don't
// distinguish between a property that exists with an undefined or null
// value, or one where the property does not exist at all.

let getObject: ('a, string) => option<object>
let getObjectBySymbol: ('a, symbol) => option<object>

let getBool: ('a, string) => option<bool>
let getBoolBySymbol: ('a, symbol) => option<bool>

let getFloat: ('a, string) => option<float>
let getFloatBySymbol: ('a, symbol) => option<float>

let getBigInt: ('a, string) => option<bigInt>
let getBigIntBySymbol: ('a, symbol) => option<bigInt>

let getString: ('a, string) => option<string>
let getStringBySymbol: ('a, symbol) => option<string>

let getSymbol: ('a, string) => option<symbol>
let getSymbolBySymbol: ('a, symbol) => option<symbol>

let getFunction: ('a, string) => option<function>
let getFunctionBySymbol: ('a, symbol) => option<function>
