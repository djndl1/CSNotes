_Protection_ and _relocation_ are the two major problems with running multiple programs in memory.

# Early Memory Management

In the early days, there is no memory abstraction. Every program simply saw the physical memory. 
A program may have multiple threads, but it cannot have multiple unrelated programs running together. 
One way to run multiple programs is to swap the one in memory to disk and load another process into memory. 
IBM 360 segmented memory into 2-KB blocks, each of which is assigned a protection key, 
with one in the PSW to indicate the active memory block. Only the OS could change the key. 
This solves the protection problem, but the relocation problem arises: absolute addresses are used in programs, 
and some absolute addresses may reference memory not belonging to the process. 
*Static relocation*, modifying the absolute address by adding base address, was introduced by IBM to solve this.
However, this slowed down the loading in order to modify the addresses, 
and sometimes it was difficult to determine whether an integer is an address.

This kind of memory protection is reintroduced into [Memory protection keys](https://lwn.net/Articles/643797/)

# Address Space

- _address space_: the set of addresses that a process can use to address memory.
   - Address space separates the memory space the process sees from the physical memory by providing a unique address space to each process.

- the base-and-limit approach: a simple way is to use base and limit registers. A program are loaded into consecutive memory location. The base register is loaded with the physical address where its program begins in memory and the limit register is loaded with the length of the program. Dynamic relocation is used. Every time a process reference memory, the CPU hardware automatically adds the base value to the address generated by the process before sending the address out on the memory bus. Only the OS can modify the base and limit registers. This gives each process a private memory space.
  - 8088 doesn't even have the limit register to prevent out-of-range memory references.
  - this differs from the protection key scheme in that any address used by the process is relative to the base register, forming an address space.

However, not all programs can be stuffed into main memory. Two general strategies exist:

- _swapping_: consists of bringing in each process in its entirety, running it for a while, then putting it back on the disk. Swapping sometimes requires memory compaction to eliminate unused holes inside memory. A process's data/stack segment can grow. It's difficult to predict how much memory a process will need. If a process requires more memory than it has, the whole process must be moved to a hole large enough for it. If this cannot be done, and swapping back to disk is impossible, this process has to be suspended.

- _virtual memory_: allows programs to run even when they are only partially in main memory.

The OS must manage memory allocation of processes (note this is not about implementation of `malloc` but the techniques are similar):

- _bitmaps_: memory is divided into allocation units with each unit assigned a bit to indicate its allocation state. However, searching through bitmaps to modified allocation states is slow.

- _linked list_: a sorted linked list of allocated and free memory segments, where a segment either contains a process or is an empty hole between two processes.  The node would look like `(State, StartAddress, SegmentLength, PointerToNextNode)`. There are several algorithms to allocate memory for a created process.
   - First fit: use the first hole that is big enough
   - Next fit: search the list from the place where it left off last time instead of always at the beginning; worse than first fit
   - Best fit: take the smallest appropriate hole among the entire list; resulting in more wasted memory than first/next fit
   - Worst fit: take the largest available hole so that the new hole will be big enough to be useful; not very good either
   - Quick fit: maintains separate lists for some of the more common sizes requested. The downside is expensive merge during deallocation due to segment nodes not being sorted.
   - Allocated segments and unallocated holes can be maintained in separate lists. The hole list may be sorted to allow best-fit to quickly find the first as the best-fit hole. The downside is inefficient deallocation where a freed segment has to be removed from 
   the process list and inserted into the hole list, with possible merging.

# Virtual Memory

The problem of programs larger than memory has been around since the beginning of computing. A solution in the '60s was to split programs into little pieces (overlays). The overlay manager loaded each overlay when it was needed. The overlays were kept on the disk and swapped in and out of memory by the overlay manager. The work of splitting the program had to be done manually by the programmer. The automatic version of this splitting method is known as _virtual memory_. The basic idea is that each program has its own address space, which is broken up into chunks called _pages_ and swapped by pages. An alternative, called _segmentation_ is to use variable-sized segment as units, which is not really used much nowadays.

Virtual memory creates a new abstraction - the address space which is an abstraction of physical memory, just as a process is an abstraction of the physical processor.

## Paging Overview

- _page_: a contiguous range of addresses. These pages are mapped onto physical memory by an _Memory Management Unit_ (MMU), but not all pages have to be in physical memory at the same time to run the program.

- _page frame_: the corresponding units in the physical memory.

- _virtual address_: program-generated address.

When virtual memory is used, the virtual addresses do not go directly to the memory bus. Instead, they go to an MMU that maps the virtual addresses onto the physical memory addresses. The MMU holds the current set of page mappings. A present/absent bit keeps track of which pages are physically present in memory. A page fault causes the OS to bring the unmapped page into physical memory and occupy a page frame if that page is not mapped. The original page that uses this frame, if any, is set to absent and the faulting page is set to present.

Using a page size of a power of 2 simplifies the mapping from virtual addresses to physical addresses as the lower offset part may be retained while the higher page number is mapped and replaced. All memory access made by software use virtual addresses including the kernel, which has its own set of mappings. Sometimes the OS maps the kernel memory into the user process' virtual space but protected to avoid switching page mappings.

## Page Table

### Table Entry

The virtual address is split into a virtual page number and an offset. The purpose of the page table is to map virtual pages onto page frames.

```
            Caching
            disabled    Modified          Present/Absent
+-----------|-----|-----|-----|----------|-----|-----------------------------+
| || || |-| |     |     |     |          |     |                             |
| || || |-| |     |     |     |Protection|     |       Page Frame Number     |
| || || |-| |     |     |     |          |     |                             |
+-----------|-----|-----|-----|----------|-----|-----------------------------+
                  Referenced
```

- page frame number: the output

- present/absent bit: whether this page/entry is mapped and can be used

- protection bits: what kinds of access are permitted

- supervisor bit: whether the page is accessible only to privileged code.

- modified bit/dirty bit: must be written back to nonvolatile storage when the OS decides to reclaim a frame

- referenced bit: whether a page is referenced. It helps the OS choose a page to evict when a page fault occurs. Pages that are not being used are far better candidates than pages that are.

- caching bit: whether caching is disabled for this page. Important for pages that map onto device registers rather than memory.

The major issue in any paging system is

1. the mapping must be fast;

In widely implemented schemes for speeding up paging and handling large virtual address spaces, the page table is in memory. Most programs tend to make a large number of references to a small number of pages. Computers are equipped with a small hardware device for mapping virtual address to physical addresses without going through the page table - _translation lookaside buffer_, also called an _associative memory_, which is usually inside the MMU and consists of a small number of entries, rarely more than 256. When the virtual page number is not in the TLB. The MMU detects the miss and does an ordinary page table lookup. It then evicts one of the entries from the TLB and replaces it with the page table just looked up.

Many RISC machines do nearly all of page management in software. The TLB entries are explicitly loaded by the OS. When a TLB miss occurs, a TLB fault causes the OS to buffer the page entry in the TLB. If the TLB is moderately large to reduce the miss rate, software management of the TLB turns out to be acceptably efficient. The OS can use some prediction methods to cache related pages. However, the page that holding the page table may not be in the TLB, causing additional TLB faults. The OS can maintain a large software cache of TLB entries in a fixed location whose page is always kept in the TLB (multi-level?). 

A _soft miss_ occurs when the page referenced is not in the TLB. A _hard miss_ occurs when the page itself is not in memory. Looking up the mapping in the page table hierarchy is known as a _page table walk_. Some misses are slightly softer than other misses. _Minor page fault_ is that the page is not in the page table the current process, but brought into memory by another process. A major page fault occurs if the page needs to be brought in from disk. The virtual address may not map to any storage, resulting in segmentation fault.

2. if the virtual address space is large, the page table will be large. And EACH process needs its own page table.

_multilevel page table_: avoid keeping all the page tables in memory all the time. The unused entries of the top level  or intermediate levels can be marked as absent, saving a lot of space for low-level page tables. Not each process need to have a full multi-level page table. Intel 386 uses a two-level page table (10-10-12). Pentium Pro extended each entry in each level of the page table from 32 bits to 64 bits. AMD64 uses a four-level page table (9-9-9-9-12).

_inverted page tables_ (Itanium): one entry per page frame in real memory. The entry keeps track of which (process, virutal page) is located in the page frame. It's hard to perform virtual-to-physical translation. TLB and hashing are used.

# Page Replacement Algorithms

TODO

# Design Issues for Paging Systems

## Local versus Global Allocation Policies

TODO

## Load Control

TODO

## Page Size

TODO

## Separate Instruction and Data Spaces

Most computers have a single address space that holds both programs and data.

PDP-11 I-space, D-space

In L1 cache, separate I-spaces and D-spaces are still common.

## Shared Pages

In a large multiprogramming system, it is common for several processes to be running the same program at the same time. If separate I- and D-spaces are supported, it is relatively straightforward to share programs by having two or more processes use the same page table for their I-space but different page tables for their D-spaces.

## Shared Libraries

When a program is linked with shared libraries, instead of including the actual function called, the linker includes a small stub routine that binds to the called function at run time. A shared library is not read into memory in a single blow. It is paged in, page by page, as needed, so functions that are not called will not be brought into RAM.

_Position-independent Code_: since shared libraries are to be located in different addresses in virtual memory space, absolute addresses cannot be used. 

## Mapped Files

Shared libraries are a special case of memory-mapped files. In most implementations, no pages are brought in at the time of the mapping, but as pages are touched. When the process exits, or explicitly unmaps the file, all the modified pages are written back to the file on disk. If two or more processes map onto the same file at the same time, they can communicate over shared memory. This mechanism provides a high-band-width channel between processes and is often used as such.

## Cleaning Policy

Paging daemon: sleeps most of the time but is awakened periodically to inspect the state of memory. If two few page frame are free, it begins selecting pages to evit using some page replacement algorithm.

# Implementation Issues

TODO

# Segmentation

For many problems, having two or more separate virtual address spaces may be much better than having only one. e.g. compilation, when code source text, parse tree, symbol tables and call stack all take part of the address space and may collide. A segment is a logical entity, which the programmer is aware of and uses as a logical entity.

To specify an address in this segmented or two-dimensional memory, the program must supply a two-part address, a segment number and number within the segment. 

Segments can facilitate linking, in which case, each procedure occupies a segment, and one recompiled procedure does not affect the others. Some shared code can occupy a segment. Different segments can have different kinds of protection.

# Implementation and Examples

One essential difference between paging and segments is that pages are of fixed size and segments are not.

## MULTICS 

TODO

## Intel x86

x86 has 16K independent segments. The heart of the x86 virtual memory consists of two tables:

- Local Descriptor Table: describes segments local to each program, including its code, data, stack, and so on.

- Global Descriptor Table: describes system segments, including the OS itself.

Each program has its own LDT, but there is only one single GDT, shared by all the programs on the machine.

To access a segment, a 16-bit selector (number) for that segment is loaded into one of the segment registers. CS for code segment, DS for data segment. 

```
           13                   1           2
+---------------------------------------------------+
|         Index             |Gdt/LDT|Privilege level|
+---------------------------------------------------+
```

(selector, offset) is converted to a physical address, selector selects the segment, offset denotes the address in the segment.

After loading the selector, the corresponding descriptor (64-bit) is fetched from the LDT or GDT and stored in microprogram registers. If the segment does not exist, or is currently paged out, a trap occurs. The hardware then uses `Limit` field to check if the offset is beyond the end of the segment. The x86 adds the 32-bit Base field in the descriptor to the offset to form _linear address_. If paging is disabled, the linear address is interpreted as the physical address and sent to the memory for read/write. With paging disabled, it's a pure segmentation scheme. If paging is enabled, the linear address is interpreted as a virtual address and mapped onto the physical address using page tables. The page tables are three-level hierarchy.



```
                            x86 Code Segment Descriptor

                                           +----------------> segment present/absent
                                           |                  in/from memory
16/32 bit segment                          |
         <---------+                       |    +-----------> privilege level
                   |                       |    |
                   |                       |    |
                   |                       |    |    +------> System/Application
                   |                       |    |    |
Limit in           |                       |    |    |
bytes/pages<---+   |                       |    |    |    +-> Segment Type and Protection
               |   |                       |    |    |    |
               |   |                       |    |    |    |
+--------------|---|-|---|---------------|-|-|--|--|-|-|--|---|------------+
|   Base 24|31| G| D | 0 |  Limit 16|19  | P | DPL | S | Type | Base 16|23 |
+--------------------|---|-------------------|-----|---|------|------------+
|                                         |                                |
|                 Base 0-15               |           Limit 0-15           |
+-----------------------------------------|--------------------------------+
```

All current OSes for the x86 has Base = 0 and Limit set to the maximum, in effect, normal paging. All the segment registers can be set up with the same selector.
