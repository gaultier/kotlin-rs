var a = 0
var b = 1

var i = 1
while (i < 35) {
    val tmp = b
    b = b + a
    a = tmp
    i = i + 1
}

println(b) // 9227465
