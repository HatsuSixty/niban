# Niban

Simple imperative programming language.  
Niban is a language that aims to be simple and low-level-ish, like C, but with convenient features that make it nicer to use.

## Dependencies

- Rust toolchain
- [`qbe`](https://c9x.me/compile)
- [`fasm`](https://flatassembler.net/)
- binutils's `as` & `ld`

## Quick Start

The compiler is written in Rust, so you can use `cargo` for building:
```console
$ cargo build
```
For building and running the test program `test_program.niban`, run the following commands:
```console
$ cargo run test_program.niban
$ ./test_program
```
