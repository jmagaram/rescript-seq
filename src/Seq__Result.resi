let fromTryCatch: (unit => 'a) => result<'a, exn>
let toOption: result<'a, 'b> => option<'a>
