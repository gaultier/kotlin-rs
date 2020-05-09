
fun whenAssign(obj: Any): Any {
    val result = when (obj) {
        1 -> "one"          
        "Hello" -> 1       
is Long -> false          
                else -> 42
   }
    return result
}

    println(whenAssign("Hello")) // Expect: 1
    println(whenAssign(3.4)) // Expect: 42
    println(whenAssign(1)) // Expect: one
