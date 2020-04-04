# Roadmap

*Roughly in order of priority, non exhaustive*

## Lexer

- [ ] Triple quote strings (multiline) e.g `""" abc """`
- [ ] Templated strings
- [ ] Templated strings nested
- [x] Simple Strings e.g `“abc”`
- [x] Char literals e.g `'a'`
- [x] Char unicode literals e.g `'\uabcd'`
- [x] Char escaped sequences literals e.g `'\r'`
- [x] All single character lex tokens e.g `}`
- [x] All multi character lex tokens e.g `!==`
- [x] Treat `\r\n` as newline
- [x] Nested comments
- [ ] Doc comments
- [x] Identifiers (potentially with unicode)
- [ ] Identifiers inside backticks e.g 
    ``` 
    `foo bar` 
    ```
- [x] Keywords
- [x] Don’t give out of bounds locations
- [x] Newline-sensitive
- [x] Annotations (e.g `@Timeout`)

## Parser

- [x] Simple AST
- [x] Math expressions e.g  `1 + 3`
- [x] Strip comments, shebang, etc from ast before parsing
- [x] Math
- [x] Boolean logic e.g `1 > 2 != 3<4`
- [x] Newline-sensitive
- [x] Range e.g `1..5`
- [x] If
- [x] When
- [x] When with subject
- [x] Range test in when entry e.g `when {4 in 1..5 -> 99 else 0}`
- [x] Type test in when entry e.g `when {4 is Int -> true else false}`
- [x] Elvis operator
- [x] As expression
- [x] While
- [x] Do while
- [x] Val declaration e.g `val a = 1`
- [x] Var declaration e.g `var a = 1`
- [x] When with subject with binding e.g `when (val x = 1) {0 -> 1; 1 -> 2; 2 -> 4; else x * 2}`
- [x] Do while with binding e.g `do {val a = 1} while (a < 10)`
- [x] Explicit types e.g `val a: Long = 1`
- [x] Prefix increment/decrement operators e.g `--a; ++a;`
- [x] Postfix increment/decrement operators e.g `a--; a++;`
- [x] Variable assignement
- [x] Break
- [x] Continue
- [x] Simple inline functions declaration without arguments e.g `fun add(): Int = a + b`
- [x] Simple C-like functions declaration without arguments e.g `fun add(): Int { a + b; }`
- [x] Function call without arguments e.g `add()`
- [x] Simple inline functions declaration e.g `fun add(a: Int, b: Int): Int = a + b`
- [x] Simple C-like functions declaration e.g `fun add(a: Int, b: Int): Unit { a + b; }`
- [x] Function call e.g `add(1, 5)`
- [x] Return in function e.g `fun add(a: Int, b: Int): Int { return a + b; }`
- [x] Explicit type for function with block body (except Unit)
- [x] In expression e.g `val a = 1 in 0..10`
- [ ] Is expression e.g `val a = 1 is Int`
- [ ] Call function with named parameter
- [ ] Default value for parameter in function
- [ ] Tailrec function
- [ ] Variadic function
- [ ] Class
- [ ] Data class
- [ ] Enum
- [ ] Shebang
- [ ] Import
- [ ] Package declaration
- [ ] Try-catch
- [ ] Finally
- [ ] Annotations
- [ ] Infix function
- [ ] For
- [ ] Special syntax for last function call argument if it is a callable e.g `values.map { it * 2}`
- [x] As expression
- [ ] Jump expression with label e.g `return@loop; break@foo; continue@bar;`
- [ ] Interface
- [ ] Type alias
- [ ] Visibility modifier
- [ ] Generic
- [ ] Contract
- [ ] Generic constraint
- [ ] Operator overloading
- [ ] Extension function
- [ ] Lambda
- [ ] Sealed class
- [ ] Inheritance
- [ ] Object (i.e singleton)
- [ ] Destructuring declaration
- [ ] Delegation
- [ ] Delegated properties

## Type checker

- [ ] `Short` type
- [ ] `UShort` type
- [ ] `Byte` type
- [ ] `UByte` type
- [ ] `Nothing` type
- [x] `Any` type
- [ ] Smart casts
- [ ] Nullable types
- [x] Type check math
- [x] Type check boolean logic
- [x] Type inference

## Name resolution

- [x] Variable
- [x] Function call
- [ ] Class resolver
- [x] Out of order resolution

## Lint

- [ ] Warning on shadowing
- [ ] Warning on unused
- [ ] Too many function arguments (255 for the JVM)

## CLI

- [ ] Simplify CLI (align with kotlinc)
- [ ] Add debug dump of the AST
- [ ] Add debug dump of resolution & types
- [ ] Show default paths of dependencies e.g stdlib
- [ ] Give on the CLI paths of dependencies e.g stdlib

## Errors

- [ ] Ternary operator
- [x] Missing parentheses around if, while, for, when clause
- [ ] Variations of a unicode literal `'\u{abcde}'` or `\uabcde`
- [ ] Show hints on error
- [x] Show full line of source code on error
- [x] Show the exact location on the line of the error with description

## Examples of using the libparsing

- [x] Add AST sexp dump
- [ ] Write examples in rust
- [ ] Write one example in another language (C? Python? Java with JNI?) -> simple stats, kotlinfmt, remove unused, loc count

## Research
- [x] Type inference
- [ ] Generics
- [ ] Java Byte Code
- [ ] Varargs
- [ ] Null safety
- [ ] Language server (?)

## Code emitting

- [ ] IR (?)
- [x] Lisp
- [ ] Jar

