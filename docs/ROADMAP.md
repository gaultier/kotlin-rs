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
- [ ] Type test in when entry e.g `when {4 is Int -> true else false}`
- [ ] Range test in when entry e.g `when {4 in 1..5 -> 99 else 0}`
- [ ] Elvis operator
- [ ] As expression
- [x] While
- [x] Do while
- [x] Val declaration e.g `val a = 1`
- [x] Var declaration e.g `var a = 1`
- [x] When with subject with binding e.g `when (val x = 1) {0 -> 1; 1 -> 2; 2 -> 4; else x * 2}`
- [ ] Do while with binding e.g `do {val a = 1} while (a < 10)`
- [ ] Explicit types e.g `val a: Long = 1`
- [x] Prefix increment/decrement operators e.g `--a; ++a;`
- [x] Postfix increment/decrement operators e.g `a--; a++;`
- [x] Variable assignement
- [ ] For
- [x] Simple inline functions declaration without arguments e.g `fun add(): Int = a + b`
- [x] Simple C-like functions declaration without arguments e.g `fun add(): Int { a + b; }`
- [x] Function call without arguments e.g `add()`
- [x] Simple inline functions declaration e.g `fun add(a: Int, b: Int): Int = a + b`
- [x] Simple C-like functions declaration e.g `fun add(a: Int, b: Int): Unit { a + b; }`
- [x] Function call e.g `add(1, 5)`
- [ ] Return in function
- [ ] Variadic functions
- [ ] Class
- [ ] Enum
- [ ] Shebang
- [ ] Import
- [ ] Package declaration
- [ ] Try-catch
- [ ] Annotations

## Type checker

- [ ] `Short` type
- [ ] `UShort` type
- [ ] `Byte` type
- [ ] `UByte` type
- [ ] `Nothing` type
- [ ] `Any` type
- [x] Type check math
- [x] Type check boolean logic
- [x] Type inference (hard)


## Name resolution

- [x] Variable
- [ ] Function call
- [ ] Class loader (hard)
- [ ] Warning on shadowing
- [ ] Warining on unused

## CLI

- [ ] Simplify CLI (align with kotlinc)

## Errors

- [ ] Error production: Ternary operator
- [ ] Error production: Missing parentheses around if, while, for, when clause
- [ ] Error production: variations of a unicode literal `'\u{abcde}'` or `\uabcde`
- [ ] Show hints on error
- [x] Show full line of source code on error
- [x] Show the exact location on the line of the error with description

## Examples of using the libparsing

- [x] Add AST sexp dump
- [ ] Write examples in rust
- [ ] Write one example in another language (C? Python? Java with JNI?) -> simple stats, kotlinfmt, remove unused, loc count

## Research
- [ ] Type inference
- [ ] Generics
- [ ] Java Byte Code
- [ ] Varargs
- [ ] Null safety
- [ ] Class loader (hard)
- [ ] Language server (?)

## Code emitting

- [ ] IR (?)
- [ ] Jar

