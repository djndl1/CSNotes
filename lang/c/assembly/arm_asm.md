Assembly language is the best choice when writing

1. bootloader

2. interrupt handler

3. low-level locking code for multi-threading

4. code for machines where no compiler exists

5. code that a compiler cannot optimize

6. on computers with very limited memory

7. code that requires low-level access to architectural and/or processor features.

 Analysis of assembly code is an important skill for C and C++ programmers, who may occasionally have to diagnose a fault by looking at the contents of CPU registers and single-stepping through machine instructions.

Without first learning assembly language, it is impossible to learn advanced concepts such as microcode, pipelining, instruction scheduling, out-of-order execution, threading, branch prediction, and speculative execution. The best programmers understand why some language constructs perform better than others, how to reduce cache misses, and how to prevent buffer overruns that destroy security.

# Memory layout

```
+-----------+  FFFFFFFF
|   ...     |
+-----------+
|  Stack    |
|           |
+-----------+
|           |
|           |
|  Unused   |
|           |
|           |
+-----------+
|  Heap     |
|           |
+-----------+
|   Data    |
|           |
+-----------+
|  Text     |
+-----------+
|  ...      |
+-----------+ 00000000
```

The Linux OS, by default, configures the ARM processor to run in little-endian mode.

Storage space for a variable can be allocated in three ways:

1. statistically: `.data` section

2. dynamically: on the heapj. The address of a dynamic variable is always stored in another pointer variable.

3. automatically: on the stack. The address of an automatic variable is computed as an offset from the stack pointer.

# GNU Assembly Syntax

An assembly program consists of four basic elements:

1. assembler directives; reserve memory, control which program section is being used; define macros; include other files; perform other operations that control assembling. All assembler directives begin with `.`.

2. label: On the first pass, the assembler builds a symbol table, which maps each label or symbol to a numerical value. On the second pass, the assembler converts the assembly instructions and data declarations into binary, using the symbol table to supply numerical values whenever they are needed.

3. assembly instructions

4. comments.

## Directives

### Selecting the Current Section

Each section has its own address counter. Sections can also be further divided into numbered subsections. Once a section has been selected, all of the instructions and/or data will go into that section until another section is selected.

- `.data subsection`: for global variables and constants which have labels;

- `.text subsection`: for executable instructions, may also contain constant data;

- `.bss subsection`: for defining data storage areas that should be initialized to zero at the beginning of program execution, often global variables. This section does not actually consume any space in the object or executable file.

If the subsection number is not specified, it defaults to zero.

- `.section name`: the programmer can create other sections. The linker must be made aware of them.

### Allocating Space for Variables and Constants

- `.byte expressions`: zero or more expressions, separated by commas.

- `.hword expressions`: half-word, 16-bit for ARM

- `.short expressions`: 16-bit`

- `.word expressions`: a word, four bytes for ARM

- `.long expressions`: four bytes for ARM

- `.asciz "string"`/`.string "string"`: each string is followedby an ASCII NULL.

- `.float flonums`/`.single flonums`: 4-byte IEEE standard single precision numbers

- `.double flonums`: 8-byte IEEE standard double precision numbers

If no expressions are given, then the address counter is not advanced and no bytes are reserved.

### Filling and Aligning

When declaring storage, it is important that words and half-words are stored on appropriate boundaries.

- `.align abs-expr, abs-expr, abs-expr`: pad the location counter to a particular storage boundary: the number of low-order zero bits the location counter must have after advancement (like, 3 for a multiple of 8), the fill value to be stored in the padding bytes (optional, defaulted to zero), maximum number of bytes that should be skipped (optional). This directive has inconsistency across architectures.

- `balign[lw] abs-expr, abs-expr, abs-expr`: adjusts the location counter to a particular storage boundary: the byte-multiple for the alignment request, the fill value (optional), maximum number of bytes that should be skipped. `w` for 2-byte word value and `l` for 4-byte long word value. Specific to GAS, has consistency across architectures.

- `.skip size, fill`/`.space size, fill`: allocate a large are of memory and initialize it all to the same value. Very useful for declaring large arrays in the `.bss` section.

It is a good practice to always to put an alignment directive after declaring character or half-word data.

```asm
        .data
i:      .word   0
j:      .word   1
fmt:    .asciz  "Hello\n"
ch:     .byte   'A','B',0
        .align 2      @ advances to the next `*00`
ary     .word   0,1,2,3,4
```
