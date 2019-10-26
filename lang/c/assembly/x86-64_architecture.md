# Overview

The need for a 64-bit x86 architecture is driven by applications that address large amount of virtual and physical memory.

## Features

- 64-bit integer capability

- additional registers to 32-bit: 16 general-purpose registers

- XMM(SSE) registers: 16

- larger virtual address space: currently 48-bit

- larger physical address space: 48-bit implementation available

- larger physical address space in legal mode: PAE mode of 52 bits of physical address.

- instruction pointer relative data access: instructions can reference data relative to the instruction pointer (RIP pointer)

- SSE/SSE2 as core instructions

- No-Execute bit: specify if a page can contain executable code or not

