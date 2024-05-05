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
For building the test program `main.txt`, run `./build.sh`:
```console
$ ./build.sh
```
