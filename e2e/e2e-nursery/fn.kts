var a = 2
fun a () = if (a>2) { 
    fun a() = a* a
    a() * a()
} else {
    fun a() = a* a*a
    a() * a()
}

println(a()) // Expect: 64
