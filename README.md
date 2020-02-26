# kotlin-rs
A work-in-progress kotlin compiler written in Rust.

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
