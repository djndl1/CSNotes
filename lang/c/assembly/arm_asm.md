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

2. dynamically: on the heap. The address of a dynamic variable is always stored in another pointer variable.

3. automatically: on the stack. The address of an automatic variable is computed as an offset from the stack pointer.

# GNU Assembly Syntax

An assembly program consists of four basic elements:

1. _assembler directives:_ reserve memory, control which program section is being used; define macros; include other files; perform other operations that control assembling. All assembler directives begin with `.`.

2. _label_: On the first pass, the assembler builds a symbol table, which maps each label or symbol to a numerical value. On the second pass, the assembler converts the assembly instructions and data declarations into binary, using the symbol table to supply numerical values whenever they are needed.

3. _assembly instructions_

4. _comments_.

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

- `ascii "string"`: no trailing ASCII NULL.

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

## Setting and Manipulating Symbols

Labels are just a kind of symbols.

- `.equ symbol, expression`/`.set symbol expression`: sets the value of `symbol` to `expression`, similar to `#define` in C.

- `.equiv symbol expression`: same as above except that the assembler will signal an error if the symbol is already defined.

- `.global symbol`/`.globl symbol`: make the symbol visible to the linker. Otherwise, symbols are visible only within the file where they are defined. GAS treats all undefined symbols as external. `.local` is needed if declared local to a file.

- `.comm symbol, length`: a common symbol, defined in more than one file, and all instances should be merged into a single symbol. `length` defines the memory allocated (the largest value will be used if there are multiple definitions).

```asm
ary:    word    0,1,2,3,4
        .equ    arysize, (. - ary) / 4   @ `.` the current address
```

## Conditional Assembly

- `.if expression`

- `.ifdef symbol`

- `.ifndef symbol`

- `.else`

- `.endif`

## Including Other Source Files

- `.include "filename`: includes supporting files at specified points in the source program.


## Macros

Macros are expanded to generate assembly code.

- `.macro macname`/`.macro macname macargs ...`: the programmer can supply a default value for any macro argument by following the name with `=deflt_val`

```asm
.macro reserve_str p1=0, p2
// called as
reserve_str x, y
```

- `.endm`: end the current macro definition

- `.exitm`: exit early from the current macro definition, used only within a `.if` or `.ifdef`.

- `\@`: a pseudo-variable used by the assembler to maintain a count of how many macros it has executed.

```asm
.macro  SHIFT   a,b
.if     \b < 0
  mov   \a, \a, asr 3-\b
.else
  mov   \a, \a, lsl #\b
.endm

.macro  enum first=0, last=5
.long   \first
.if     \last-\first
  enum  "(\first+1)",\last
.endif
```

# Architecture

ARMv4T (Thumb 16bit) -> ARMv5TE (DSP-type operations and saturated arithmetic) -> ARMv6 (unaligned memory access, multi-core support, SIMD operations within 32-bit registers, Thumb-2, TrustZone) -> ARMv7-A (mandatory Thumb-2 and NEON). Almost all architecture changes are backwards-compatible, meaning software written for the ARMv4T architecture can still be used on ARMv7 processors.

ARMv7 contains two main instruction sets, of which Thumb is a 16-bit long subset of the most commonly used 32-bit ARM instructions. The main reason for using Thumb code is to reduce code density. Thumb-2 is not an instruction set. It extends the original 16-bit Thumb instruction set to include 32-bit instructions to allow Thumb code to achieve performance similar to ARM code. NEON shares register files with VFP, and supports 8, 16, 32 and 64-bit integer and single-precision floating point data, operated on as vectors in 64-bit and 128-bit registers. Cortex-A only supports internal coprocessors.

A number of key points are common to the Cortex-A family:

1. 32-bit RISC core, with $16 \times 32$ visible registers with mode-based register banking

2. Modified Harvard Architecture (separate, concurrent access to instructions and data)

3. Load/Store Architecture

4. Thumb-2 as standard

5. VFP and NEON options

6. Backward compatibility with code from previous ARM processors

7. SMP support

8. Unaligned memory access

9. Big-endian and little-endian data access support

10. Physically indexed, physically tagged data caches

The ARM has a non-priviledged user mode and six priviledged mdoes:

1. FIQ: entered on an FIQ interrupt exception

2. IRQ: entered on an IRQ interrupt exception

3. Supervisor (SVC): on reset or when a supervisor call instruction is executed

4. Abort (ABT): on a memory access exception

5. Undef (UND): when an undefined instruction executed

6. System (SYS): mode in which the OS runs, sharing the register view with user mode

Processors that implement the TrustZone extension (Monitor/MON mode) achieve system security by dividing all of the hardware and software resources for the device so that they exist in either the _Secure world_ for security subsystem or the _Normal world_ for everything else. The _Virtulization Extensions_ add a hypervisor mode (Hyp) in addition to the existing privileged modes. If the Virtualization Extensions are implemented there is a privilege model different to that of previous architectures. A general purpose Operating System, such as Linux, and its applications, are expected to run in Non-secure state. The Secure state is expected to be occupied by vendor-specific firmware, or security-sensitive software.

- `r0`-`r12`: general-purpose registers

- `r15`/`pc`: program counter

- `r14`/`lr`: link register, holding the return address for subroutines.

- `r13`/`sp`: stack pointer. On most systems, the stack grows downwards. The use of `r13` as the stack pointer is a programming convention.

- `r11`/`fp`: frame pointer, used by high-level language compilers to track the current stack frame.

- `r12`/`ip`: inter-procedure scratch register, used by the C library when calling functions in dynamically linked libraries.

- `CPSR`: Current Program Status Register. The first four flags `N`, `Z`, `C` and `V` (overflow) are the _condition flags_. In user mode, a restricted form of CPSR called the Application Program Status Register (APSR) is accessed instead. The core can change between modes using instructions that directly write to the CPSR mode bits. More commonly, the processor changes mode automatically as a result of exception events.

- `SPSR`: Saved Program Status Register, a saved copy of the CPSR fro mthe previously executed mode.

Depending on which mode the software is executing in and the register being accessed, a register might correspond to a different physical storage location. This is called _banking_. In all modes, 'Low Registers' and R15 share the same physical storage location. `r15`  always points eight bytes ahead of the current instruction in ARM state and four bytes ahead of the current instruction in Thumb state, a legacy of the three stage pipeline of the original ARM1 processor. 

# ARM Assembly Instructions

A RISC processor, unlike CISC processors that have significant amounts of internal logic that decode machine instructions to sequence of internal operations (microcode), have a smaller number of more general purpose instructions. ARM instructions typically have a two or three operand format.

## The ARM instruction sets

A word = 32 bits. 

An explicit instruction is used to change between instruction sets. Calling functions that are compiled for a different state is known as interworking. 

For Thumb assembly code, there is often a choice of 16-bit and 32-bit instruction encodings, with the 16-bit versions being generated by default. The .W (32-bit) and .N (16-bit) width specifiers can be used to force a particular encoding (if such an encoding exists)

All GCC inline assembly code types can be built for Thumb or ARM depending on GCC configuration and command line switches.
