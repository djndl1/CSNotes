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

## Memory Mapping

- CR3 (Control Register 3): a pointer to the top level of a hierarchical collection of tables in memory which define the translation from virtual addresses to physical addresses. CR3 is filled with the address of the top level table in the hierarchy (Page Map Level 4).

A virtual or logical address is broken into 6 fields:

```
   63-48               47-39            38-30           29-21           20-12          11-0
+----------------|-----------------|----------------|-------------|-----------------|----------+
|                |                 |                |             |                 |          |
+----------------|-----------------|----------------|-------------|-----------------|----------+
     unused          PML-4 index     page directory  page directory page table index  page offset
                                     pointer index      index
```

Addresses are 8 bytes so a page stores 512 entries. TODO

- PML-4 index: for a virtual address, an index into the PML4 table. PML4 table is an array of 512 pointers that point to pages of memory. 

- Page directory pointer index: an array of 512 pointers:

- Page directory table:

- Page offset:

The CPU designer have added support for large pages using three levels of the existing tranlation tables, yielding a maximum of $2^{21}$ bytes.

Fast lookup is done through TLB (Translation Lookaside Buffer) after a page translation has been performed and added to the TLB. Typical miss rates are from 0.01% to 1%.
