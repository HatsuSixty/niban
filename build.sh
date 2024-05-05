#!/bin/bash

set -xe

# Generate QBE code
cargo run > test_program.ssa

# Generate assembly from QBE
qbe test_program.ssa > test_program.s

# Generate main object file
as test_program.s -o test_program.o

# Generate runtime library
fasm runtime_linux.asm

# Link main executable with runtime library
ld -o test_program test_program.o runtime_linux.o
