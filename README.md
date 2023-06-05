# Sergelang
Sergelang is a programming language for the final project of the course "Compiler Principle" at the Zhejiang University.
## Syntax

## Usage
### Run
```bash
# assume your riscv64 gcc toolchains is in /usr/riscv64-linux-gnu
cd runtime
make
cd ..
cargo run -- examples/test.rs
qemu-riscv64 -L /usr/riscv64-linux-gnu ./build/output
```
