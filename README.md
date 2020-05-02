# kotlin-rs
A work-in-progress Kotlin compiler written in Rust.

The goals of the project are:

- An alternative to the official Jetbrains compiler
- Non-JVM
- Can be used as a library (for example to build a language server or developer tools), or as a stand-alone executable (which uses the library)
- Fast 
- Beautiful error messages
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

# A class file called `FibonnaciRec.class` was created:
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
$ kotlin asm -f fibonnaci_iter.kts
9227465

# No JVM involved here, it is a stand-alone executable:
$ file ./fibonnaci_iter
./fibonnaci_iter: Mach-O 64-bit executable x86_64

# We can run it directly like any other executable:
$ ./fibonnaci_iter
9227465
```

This project can also be used as a library.

## Troubleshooting when working on the compiler

```
# Possible values: 0|trace|debug|info|warn|error
$ export RUST_LOG=debug

# Possible values: 0|1|full
$ export RUST_BACKTRACE=full

# Dump the output of the lexer
$ cargo run -- dump_tokens -f /path/to/file.kt

# Dump the output of the parser
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

Pre-pre alpha. Do not use yet. Only a small subset of the language is supported (basically only expressions, statements and functions, and we only consider the current file when compiling. No classes, no generics), and there are likely bugs.

Some full-fledged examples compile though, see the `e2e` directory or the tests.

See the [roadmap](docs/ROADMAP.md).
