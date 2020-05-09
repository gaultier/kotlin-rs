fun fibonacci(n: Int): Int = if (n < 2) n   else
fibonacci(n-1) + fibonacci(n-2)

println(fibonacci(35)) // Expect: 9227465
