
fun cases(obj: Any) {
    when (obj) {
        1 -> println("One")
        "Hello" -> println("Greeting")
        is Long -> println("Long")
        !is String -> println("Not a string")
        else -> println("Unknown")
    }
}

cases("Hello") // Expect: Greeting
cases(1) // Expect: One
cases(0L) // Expect: Long
cases("hello") // Expect: Unknown
