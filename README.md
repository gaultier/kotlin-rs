# kotlin-rs
A work-in-progress Kotlin compiler written in Rust.

The goals of the project are:

- An alternative implementation of a compiler for the Kotlin language (or at least a large subset of it)
- Non-JVM
- Can be integrated as a library (for example to build a language server or developer tools)
- Fast 
- Produces beautiful error messages
- Heavily tested

## Quick start
Prerequisite: a Rust toolchain installed.

```sh
# Build
$ cargo build --release
# Run with a file
$ cargo run -- build /path/to/file.kt
# Or with stdin
$ echo '123' | cargo run -- build
```

## Status

Pre-pre alpha. Do not use yet. See `docs/ROADMAP.md`.
