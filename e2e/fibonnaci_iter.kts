var a = 0
var b = 1

var i = 0
while (++i < 20) {
    val tmp = b
    b += a
    a = tmp
}

// println(b)
b
