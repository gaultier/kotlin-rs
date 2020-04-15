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

# The statically built, self-contained executable can be found in `./target/release/kotlin`.
# It will be now referred as `kotlin`.
# Run with a file:
$ kotlin -f e2e/fibonnaci_rec.kts
10946

# Or with stdin:
$ echo 'println(4*5)' | kotlin
20

# Format a file (it does not modify the file in place, just prints on stdout):
$ kotlin fmt -f e2e/fibonnaci_iter.kts

```

This project can also be used as a library.

## Debugging

```
# Possible values: 0|trace|debug|info|warn|error
$ export RUST_LOG=debug
# Possible values: 0|1|full
$ export RUST_BACKTRACE=full
$ cargo run -- dump_tokens -f /path/to/file.kt
$ cargo run -- dump_ast -f /path/to/file.kt
# Prettier view of the ast with s-expressions
$ cargo run -- sexp -f /path/to/file.kt
```

## Tests

`cargo test`

## Status

Pre-pre alpha. Do not use yet.

Some full-fledged examples compile though, see the `e2e` directory.

See the [roadmap](docs/ROADMAP.md).
