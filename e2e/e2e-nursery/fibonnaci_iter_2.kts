var a: Int = 0
var b: Int = 1

var i: Int = 0
var tmp: Int = 0
while (++i < 35) {
    tmp = b
    b = a + b
    a = tmp
}

println(b) // Expect: 9227465
