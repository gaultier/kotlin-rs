fun odd(n: Int): Boolean = if (n == 1) true else even(n-1)

fun even(n: Int): Boolean = if (n == 0) true else odd(n-1)

println(even(100))
