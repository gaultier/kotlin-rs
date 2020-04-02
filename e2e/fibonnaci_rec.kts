fun fibonacci(n: Int): Int = if (n <= 1) 1   else
fibonacci(n-1) + fibonacci(n-2)

println(fibonacci(20)) // 10946
