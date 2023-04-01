let fromSeed: ('state, 'state => option<('a, 'state)>) => array<'a>
let fromOption: option<'a> => array<'a>
@val external fromOneValue: 'a => array<'a> = "Array.of"
