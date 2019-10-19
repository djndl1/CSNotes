Modern C/C++ compilers do excellent optimization and beginning assembly programmers are no match for a good compiler. It is unlikely for assembly to outperform C/C++ for most general purpose tasks. One advantage of assembly language is that it can do things not possible in high level languages.

> A typical C program has a `main` function which is called indirectly via a `_start` function in the C library.

# Numbers

binary number representation: `1001001010b`

## Floating-point


```
sign bit | exponent | fraction |
```

A number with exponent field equal to 0 is defined to be 0. It is possible to store a negative 0. An exponent of all 1s is used to mean either negative or positive infinity.

# Memory

The hardware mapping registers on an x86-64 CPU can map pages of 2 different sizes - 2MB and 4096 bytes (for kernel and for other cases respectively in Linux). The operation of the memory system is to translate the upper bits of the address from a process's logical address to a physical address.

In Linux, memory for a process is divided into 4 logical regions:

- text: starting at 0 (0x400000 in an x86-64) `.text`

- data: above text `.data`; above data there  is `.bss`, which has uninitialized symbols, thus not stored in the executables.

- heap: dynamically resizable region of memory; also used for loaded shared object

- stack: high address (0x7fffffffffff) (47bits); restricted in size by the Linux kernel, typically to 16 MB. 

```bash
cat /proc/$$/maps
```

String in `yasm` do not end in null bytes automatically.

## Use `gdb`to example memory

```bash
p expression
p/FMT expression

x/NFS address // NFS = N Format Size
```
