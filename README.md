# kotlin-rs
A work-in-progress kotlin compiler written in Rust.

The goals of the project are:

- An alternative implementation of a compiler for the Kotlin language
- Non-JVM
- Can be integrated as a library (for example to build a language server)
- Fast 
- Produces beautiful error messages

## Quick start
Prerequisite: a Rust toolchain installed.

```sh
$ cargo build --release
# With a file
$ cargo run -- build /path/to/file.kt
# Or with stdin
$ echo '123' | cargo run -- build
```

## Status

Pre-pre alpha. Do not use yet.

- [ ] Lexer: WIP
- [ ] Parser
