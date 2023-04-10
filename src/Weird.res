@genType
let o = Extras__Task.make(~promise=() => Js.Promise2.resolve(11), ~onError=_ => 3)

@genType
let e = Extras__TaskResult.make(~promise=() => Js.Promise2.resolve(11), ~onError=_ => 3)
