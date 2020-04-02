fun printMessage(message: String): Unit {                               // 1
    println(message)
}


fun sum(x: Int, y: Int): Int {                                          // 3
    return x + y
}

fun multiply(x: Int, y: Int) = x * y                                    // 4

fun main() {
    printMessage("Hello")                                               // 5                    
    println(sum(1, 2))                                                  // 9
}
