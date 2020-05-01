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
# Build an optimized, self-contained executable: `./target/release/kotlin`, thereafter referred as `kotlin`
$ cargo build --release

# By default our compiler targets the JVM, and only requires the JRE to be installed to run class files.
# Run with a file:
$ cd e2e && kotlin -f fibonnaci_rec.kts
9227465

# This creates a class file called `FibonnaciRec.class`:
$ javap ./FibonnaciRec.class
Compiled from "fibonnaci_rec.kts"
class FibonnaciRec {
  FibonnaciRec();
  static int fibonacci(int);
  public static void main(java.lang.String[]);
}

# We can run it directly with `java`:
$ java FibonnaciRec
9227465

# We can also simply use stdin, this will create `Stdin.class` and run it with `java`:
$ echo 'println(4*5)' | kotlin
20

# Experimental: format a file.
# It does not modify the file in place, just prints the formatted version on stdout
$ kotlin fmt -f fibonnaci_iter.kts

# There's also (very experimental) native support!
# This will create a native executable (macOS, x86_64 only for now) and run it.
# The assembler `nasm` is required (`brew install nasm`)
$ echo 'println(5 * 3 + 7)' | kotlin asm
22

# No JVM involved here, it is a stand-alone executable:
$ file ./Stdin
Stdin: Mach-O 64-bit executable x86_64

# We can run it directly like any other executable:
$ ./Stdin
22

```

This project can also be used as a library.

## Troubleshooting when working on the compiler

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

```sh
$ cargo test
# optional: clean up test files
$ make clean
```

## Status

Pre-pre alpha. Do not use yet. Only a small subset of the language is supported (basically everything except classes, generics, and using stuff outside the current file), and there are likely bugs.

Some full-fledged examples compile though, see the `e2e` directory or the tests.

See the [roadmap](docs/ROADMAP.md).
