fun eatACake() = println("Eat a Cake")
fun bakeACake() = println("Bake a Cake")

var cakesEaten = 0
var cakesBaked = 0

while (cakesEaten < 3) {
    eatACake()
    cakesEaten ++
}

do {
    bakeACake()
    cakesBaked++
} while (cakesBaked < cakesEaten)
// Expect: Eat a cake
// Expect: Eat a cake
// Expect: Eat a cake
// Expect: Bake a cake
// Expect: Bake a cake
// Expect: Bake a cake
