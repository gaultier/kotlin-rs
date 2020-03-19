# Roadmap

## Lexer

- [ ] Triple quote strings (multiline)
- [ ] Templated strings
- [ ] Templated strings nested (?)
- [ ] Simple Strings (e.g `“abc”`)
- [x] Char literals (e.g `'a'`)
- [x] Char unicode literals (e.g `'\uabcd'`)
- [x] Char escaped sequences literals (e.g `'\r'`)
- [ ] All single character lex tokens (e.g `}`)
- [ ] All multi character lex tokens (e.g `!==`)
- [ ] Check that `\r\n` line terminated files are handled correctly
- [ ] `Short` type
- [ ] Nested comments
- [ ] Doc comments
- [x] Don’t give out of bounds locations
- [ ] Support character literals (e.g \r \n \b \t etc)
- [x] Support literal unicode points (e.g \uFFFF)
- [x] Newline-sensitive
- [ ] Annotations (e.g `@Timeout`)

## Parser

- [x] Simple AST
- [x] Math expressions e.g  `1 + 3`
- [x] Strip comments, shebang, etc from ast before parsing
- [x] Math
- [x] Boolean logic e.g `1 > 2 != 3<4`
- [ ] Newline-sensitive
- [ ] If
- [ ] For
- [ ] When
- [ ] Elvis operator
- [ ] While
- [ ] Variable declaration
- [ ] Variable assignement
- [ ] Simple inline functions declaration e.g `fun add(a: Int, b: Int): Int = a + b`
- [ ] Simple C-like functions declaration e.g `fun add(a: Int, b: Int): Int { return a + b; }`
- [ ] Function call

## Name resolution

- [ ] Variable
- [ ] Function call

## CLI

- [ ] Simplify CLI (align with kotlinc)

## Errors

- [ ] Error production: Ternary operator
- [ ] Error production: Missing parentheses around if, while, for, when clause
- [ ] Error production: variations of a unicode literal `'\u{abcde}'` or `\uabcde`
- [ ] Show hints on error
- [x] Show full line of source code on error
- [x] Show the exact location on the line of the error with description

## Code emitting

- [ ] Jar

## Examples of using the libparsing

- [ ] Write examples in rust
- [ ] Write one example in another language (C? Python? Java with JNI?) -> simple stats, kotlinfmt, remove unused, loc count

## Type checker

- [x] Type check math
- [x] Type check boolean logic
- [ ] Type inference (hard)

## Research
- [ ] Type inference
- [ ] Generics
- [ ] Java Byte Code
- [ ] Varargs
- [ ] Null safety
