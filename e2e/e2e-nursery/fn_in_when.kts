when {
    true -> {
        fun square(n: Int)  = n * n
        square(5) // Expect: 25
    }
    false -> 0 
}
