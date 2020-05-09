fun printMessage(message: String): Unit {
    println(message)
}



fun sum(x: Int, y: Int): Int {
    return x + y
}

fun multiply(x: Int, y: Int) = x * y

    printMessage("Hello") // Expect: Hello
    println(sum(1, 2)) // Expect: 3
    println(multiply(3, 2)) // Expect: 6
