println(if (1 < 2) 'o' else 'x') // Expect: o
if (1 < 2) println('o') else println('x') // Expect: o


println(if (2 < 2) 'o' else 'x') // Expect: x
if (2 < 2) println('o') else println('x') // Expect: x
