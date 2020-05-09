fun sum(n: Int, acc: Int): Int = if (n == 0) acc else sum(n - 1, n + acc)

fun main() {
    println(sum(20, 0))
}
