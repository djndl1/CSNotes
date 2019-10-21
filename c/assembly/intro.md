https://www.codeproject.com/Articles/45788/The-Real-Protected-Long-mode-assembly-tutorial-for


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

# Registers

- 16 general-purpose 64-bit registers: `rax`, `rbx`, `rcx`, `rdx`, `rsi`, `rdi`, `rbp`, `r8`-`r15`. The old 32-bit and 16-bit registers are still available as lower part of the 64-bit registers: `eax`/`ax`, `ebx`/`bx` ... `r8b`/`r8w`,`r8d`

- 16 modern floating-point registers (128- or 256-bit)

- 64-bit instruction pointer register `rip` (PC)

- 64-bit flag register `rflags`: currently only 32 bits (`eflags`) are used. The flag register is usually not referred to directly. Conditional instructions are used which internally access  or more flags of the flag register to determine what action to take.

Software can access the 64-bit registers as 64-bit, 32-bit, 6-bit and 8-bit values.

## Moving Data

Immediate operands can be 1, 2, or 4 bytes for most instructions. `mov` allows 8 byte immediate values.

```assembly
mov     rax, 100
mov     eax, 100 // shorted, sometimes preferred.
```

Moving a 32-bit constant into a 64-bit register will clear out the top half. 

```assembly
mov   rax, [a]  ; load from memory
```

Also

- `movzx`: move and zero extend

- `movsx`: move and sign extend

```assembly
movsx   rax, byte [data] ; move byte, sign extend
movzx   rbx, word [sum]  ; move word, zero extend
movsxd  rcx, dword [count] ; move dword, sign extend
```

```assembly
mov   [a], rax  ; move data from rax to a
mov   rbx, rax  ; move data from rax to rbx
```

# Arithmetic

- `neg`: two's complement of its operand, which can be either a general-purpose register or a memory reference; it sets the sign flag (SF) and the zero flag (ZF).

```assembly
neg   rax
neg   dword [x]
neg   byte  [x]

```

- `add`: add the contents of the source to the destination; the source can be an immediate value of 32 bits, a memory reference or a register. Only one of the operands can be a memory reference. `SF` is set to the sign bit of the result; the zero flag `ZF` is set if the result is 0; the overflow flag (OF) is set if the addition overflows.

- `inc`: increment

- `sub`: substract the contents of the source from the destination

- `dec`

- `mul`: unsigned integer multiplication

- `imul`: signed integer multiplication

1. `quad` multiplies `rax` by the source (register or memory reference)and stores the result in `rdx`(high):`rax`(low);

2. multiplies the destination by the source

3. multiplies the first source by the second (register or memory) and store in the destination

`CF` and `OF` flags are set when the product exceeds 64 bits, unless a smaller multiply is requested.

- `idiv`/`div`: takes `rdx`:`rax` as the dividend and takes a single operand (register or memory reference)

There are a collection of conditional move instructions which can be used profitably rather than using branching.

- `cmovnz`/`cmovz`: move if zero flag (not) set

- `cmovl`/`cmovlz`: move if the result was negative (or zero)

- `cmovg`/`cmovge`: move if the result was positive (or zero)

```assembly
; abs()
mov rbx, rax
neg rax
cmovl rax, rbx
```

If a value from memory is used in more than 1 operation, it might be faster to move into a register first.
