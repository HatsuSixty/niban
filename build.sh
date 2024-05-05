#!/bin/bash

set -xe

# Generate QBE code
cargo run > main.ssa

# Generate assembly from QBE
qbe main.ssa > main.s

# Generate main object file
as main.s -o main.o

# Generate runtime library
fasm runtime_linux.asm

# Link main executable with runtime library
ld -o main main.o runtime_linux.o
