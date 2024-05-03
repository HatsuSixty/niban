#!/bin/bash

cargo run > main.ssa
qbe main.ssa > main.s
as main.s -o main.o
ld -o main main.o -lc -dynamic-linker /lib64/ld-linux-x86-64.so.2
