type rec t<'a> = Done('a) | Work(unit => t<'a>)
let work: (unit => t<'a>) => t<'a>
let resolve: 'a => t<'a>
let solve: t<'a> => 'a
