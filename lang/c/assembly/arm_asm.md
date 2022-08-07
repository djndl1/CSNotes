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

- `.short expressions`: 16-bit

- `.word expressions`: a word, four bytes for ARM (32-bit from the beginning)

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

- `SPSR`: Saved Program Status Register, a saved copy of the CPSR from the previously executed mode.

Depending on which mode the software is executing in and the register being accessed, a register might correspond to a different physical storage location. This is called _banking_. In all modes, 'Low Registers' and R15 share the same physical storage location. `r15`  always points eight bytes ahead of the current instruction in ARM state and four bytes ahead of the current instruction in Thumb state, a legacy of the three stage pipeline of the original ARM1 processor. 

# ARM Assembly Instructions

A RISC processor, unlike CISC processors that have significant amounts of internal logic that decode machine instructions to sequence of internal operations (microcode), have a smaller number of more general purpose instructions. ARM instructions typically have a two or three operand format.

## The ARM instruction sets

A word = 32 bits. 

An explicit instruction is used to change between instruction sets. Calling functions that are compiled for a different state is known as interworking. 

For Thumb assembly code, there is often a choice of 16-bit and 32-bit instruction encodings, with the 16-bit versions being generated by default. The .W (32-bit) and .N (16-bit) width specifiers can be used to force a particular encoding (if such an encoding exists)

All GCC inline assembly code types can be built for Thumb or ARM depending on GCC configuration and command line switches.

## Basics

Nearly all instructions can be conditional, with a _condition modifier_, which controls at runtime whether or not the instruction is actually executed.. Load and Store instructions never set the flags. For data processing operations, a suffix `s` can be added to set the flags. `cmp`, `cmn`, `tst` and `teq` always set the flags.

```asm
/* min(R1, R2), slow on processors like the Cortex-A9 */
cmp     r0, r1
movge   r2, r1
movlt   r2, r0
```

ARM allows immediate values inside an instruction or in a _literal pool_. `=<immediate|symbol>` is used to specify any immediate 32-bit number or value of any symbol, used with load instructions. `#<immediate|symbol>` is used to specify immediate data values of data processing instructions. For immediate values that can cannot be constructed by shifting or rotating and complementing an 8-bit value, the programmer must use an `ldr` pseudo-instruction with the `=<immediate|symbol>` to specify the value.

In ARM instruction set, only 12 bits are available to specify an immediate value, with the first four bits as the rotate value, which enables the remaining eight bits to be rotated right by a number of places from 0 to 30 in steps of 2. Some immediate values cannot be constructed this way, but there are pseudo-instructions to do whatever is necessary to generate the required constant. For Thumb, constant valeus are more restricted.

A literal pool is an area of constant data held within the code section, typically after the end of a function and before the start of another, of which placement can be arranged using `.ltorg`.

- `mov32`: consists of a pair of `movw reg, movt reg` to move a 32-bit constant. It's always possible to alternative `ldr Rn, <constant>` or `ldr Rn, =label`.

Thumb-2 has an `If-Then` (IT) instruction, providing conditional execution for up to four consecutive instructions. Instructions within an IT block must also specify the condition code to be applied. Typically, IT instructions are auto-generated by the assembler, rather than being hand-coded.

ARM cores can only perform data processing on registers, never directly on memory. Data processing instructions, for the most part, use one destination register and two source operands.

```asm
Operation{cond}{s}  Rd, Rn, Operand2
```

## Load/Store

### Addressing

Register addressing

- `[Rn]`

Pre-indexed

- `[Rn, #±<offset_12>]`

- `[Rn, ±Rm]`

- `[Rn, ±Rm, <shift_op> #<shift>]`: `Rn` + shifted `Rm`

Pre-indexed with write-back

- `[Rn, #±<offset_12>]!`: the address used is stored in `Rn` after the load/store

- `[Rn, ±Rm]!`

- `[Rn, ±Rm, <shift_op> #<shift>]!`: 

- Post indexed with wirte-back

- `[Rn], #±<offset_12>`: `Rn` used as the address, `Rn` is then updated.

- `[Rn], ±Rm`

- `[Rn], ±Rm, <shift_op> #<shift>`

A pseudo-instruction

- `[Rn], =<immediate|symbol>`: A `mov` instruction or a load of a symbol in a literal pool.

### Load/Store Registers

A size can be specified: `b` (byte), `h` (half-word), `sb` (signed byte), `sh` (signed half-word). Signed versions are for sign extension. Unsigned numbers are zero-extended.

- `ldr`/`str`; load/store a single register

- `ldm`/`stm`: load/store multiple registers, used to store registers on the stack, and for copying blocks of data. The operation proceeds from the lowest register to the highest in the list. By convention, only the _Full Descending_ (FD) option is used for stacks in ARM processor-based systems. The stack pointer points to the last filled location in the stack. Nearly only used are `stmfd` and `ldmfd`. =!= specifies that the register be modified after the registers are stored.

```asm
/* transfer a memory block */
  ldr     r8, =source @ Load the address of source to r8
  ldr     r9, =dest   @ load the address of dest to r9
  ldmia   r8, {r0-r7} @ load eight word from source to r0-r7
  stmia   r9, {r0-r7} @ store these eight to dest
```

```asm
/* push to the stack */
stmfd sp!, {r0-r9}  @ equiv to `stmdb`

/* pop them */ 
ldmfd sp!, {r0-r9}  @ equiv to `ldmia`
```

- `swp`/`swpb`: atomic load and store. Deprecated, works only on uni-processor systems.

- `ldrex`/`strex`: exclusive load/store. `strex` stores only if the tagging is valid. It sets a bit to indicate whether the store succeeded.


```asm
  ldr   r12, =sem @ semaphore address
  ldr   r1, =LOCKED
splck:
  ldrex r0, [r12]
  cmp   r0, r1
  strexne r0, r1, [r12]
  cmpne r0, #1
  beq   splck
```

### Branch Instructions

- `b`: branch, the target label can be any label in the current file, or any label that is defined as `.globl` in any file that is linked in.

- `bl`: It copies the current program counter to the link register before branching (unlike x86, which push this address to the stack), Used to call procedures. To return from a procedure, `move pc, lr`.
  thus, a procedure needs to push the link register to the stack and pop it before return. 

```asm
stmfd sp!, {lr}   @ save the return address to the stack

mov r0, =fmt_string
bl  printf

ldmfd sp!, {lr}  @ pop the return address back to the link register
mov   pc, lr    @ return
```

### Pseudo-instruction

- `ldr`

- `adr`/`adrl` (long): more efficient than `ldr rx, =label`. Translated into add or subtract operations and do not require a load from memory. The address must be in the same section. They cannot be used to load addresses of labels in the `.data` section.

### Data Processing

The data procesing instructions operate only on CPU registers. Most of them use two source operands and one destination register.

```asm
Operation{cond}{S} Rd, Rn, Operand2
```

`Rn` must always be a register, `Operand2` can be an /immediate value/, a /register/, or a register shifted by an immediate value or register.

#### Comparison

Comparison operations semantically performs an arithmetic operation but discard the actual result.

- `cmp`: compare `Rn - Operand2`

- `cmn`:  compare negative`Rn + Operand2`

- `tst`: test bits `Rn and operand2`

- `teq`: test equivalence `Rn xor operand2`

```asm
if (a & 1) {
  a += 3;
} else {
  a += 7;
}

tst     r4, #1
addne   r4, r4, #3
addeq   r4, r4, #7
```

#### Arithmetic

- `add`, `adc`: add (with carry)

- `sub`, `sbc`: subtract (with carry)

- `rsb`, `rsc`: reverse subtract (with carry)

```asm
    .data
fmt:    .string "The sum is %d\n"
    .align

x:  .word   5
y:  .word   8

    .text
    .global main

main:
    push    {r11, lr}
    mov     r11, sp

    adr     r1, .x_addr
    ldr     r1, [r1]
.x_load:
    add     r1, r1, pc
    ldr     r1, [r1]


    adr     r2, .y_addr
    ldr     r2, [r2]
.y_load:
    add     r2, r2, pc
    ldr     r2, [r2]

    add     r1, r1, r2
    adr     r0, .fmt_addr
    ldr     r0, [r0]
.fmt_load:
    add     r0, r0, pc
    bl      printf
    mov     r0, #0

    pop     {r11, pc}

.fmt_addr: .word    fmt-(.fmt_load+8)
.x_addr: .word   x-(.x_load+8)
.y_addr: .word   y-(.y_load+8)
```

#### Logical

- `and`, `orr`, `eor` (exclusive OR), `orn` (or and then not), `bic` (bit clear, `Rn and (not operand2)`) 

#### Data Movement

Copy data from one register to another

- `mov`, `mvn`, `movt` (copiees 16 bits into the upper 16 bits of a register)

#### Multiplication

##### 32-Bit

- `mul`

- `mla`: `r0 = r1 * r2 + r3`

##### 64-Bit

- `smull`, `umull`: signed/unsigned multiplication

- `smlal`, `umlal`: signed/unsigned multiplication and accumulate

#### Division

- `sdiv`, `udiv`: signed/unsigned

#### Special Operations

- `clz`: count leading zeros

- `mrs`: move status to register; `msr`: move register to status. Used to access the status bit of the `CPSR`, `SPSR`.

- `swi`: software interrupt. Used to perform a syscall in `/usr/include/syscall.h`. Under linux, the syscall number is put in `r7` instead of as a parameter.

- `bx`, `blx`: branch and change to thumb mode.

#### Pseudo-Instructions

Provided by the assembler

- `nop`: `mov r0, r0`

#### Bit Operations

ARM assembly does not require explicit shift instructions. The `mov` instruction can be used for shifts and rotates.

Aliases for `mov`

- `lsl`/`lsr`: logical left shift

- `asr`: arithmetic right-shift

- `ror`/`rrx`: rotate-right (extended)

```asm
mov r0, r1, lsr #2   @ R0 = R1 >> 2

add r0, r0, r0, LSL #2  @ R0 *= 5
```

Apart from multiplication and division, another common use for shifted operands is array index:

```asm
/* R1: base; R2: index*/

```

There is no scope to multiply by an immediate value. Multiplies operate only on values in registers.

Integer SIMD provides the ability to pack, extract and unpack 8-bit and 16-bit quantities within 32-bit registers and to perform arithmetic operations with a single instruction. Subword quantities in each register are operated on in parallel and the GE flags are set or cleared according to the results of the instruction to indicate overflow.

### Flow Control

#### Selection

- perform an operation that updates the `CPRSR` falgs

- use conditional execution to select a block of instructions to execute.

```assembly
cmp r0, r1
movlt r0, #1
movge r0, #0
```

```assembly
cmp r0, r1
bge else
mov r0, #1
b after
else: mov r0, #0
after: ...
```
#### Iteration

The transfer of control from a statement in a sequence to a previous statement in the sequence.

##### Pre-Test Loop

```assembly
loop: cmp r0, r1
      blt done
      ...
      b loop
done:
    ...
```

##### Post-Test Loop

A post-test loop requries only one label and one branch instruction but executes at least once while a pre-test loop needs two labels, a conditional branching and an unconditional branching.

```assembly
loop:
  ...
  
  cmp r0, r1
  blt loop
```

### Functions

Put the parameters in arguments and onto the stack, and =bl= the function.

#### Calling Conventions

- The first four arguments in `r0`, `r1`, `r2`, `r3` and the rest are pushed to the stack.

- the return value is in `r0` and possibly in `r1`, `r2`, `r3
`

- `r4-r11` are callee-saved

```asm
        push    {r11, lr} @ save link register and then frame register, ordered by register number
        mov     r11, sp   @ update frame register
        
        pop     {r11, pc} @ epilogue
```

#### Aggregate types

##### Array

```asm
@ copy an array

sub sp, sp, #400
mov r0, #0
mov r1, #0

loop:
  str r1, [sp, rf0, lsl #2]
  add r0, r0, #1
  cmp r0, #100
  blt loop
```

##### Structure/Record

```C
struct student {
  char first_name[50];
  char last_name[30];
  unsigned char class;
  int grade;
}

void func()
{
  struct student newStudent;
  
  strcpy(newStudent.first_name, "Sam");
  strcpy(newStudent.last_name, "Smith");
  newStudent.class = 2;
  newStudent.grade = 88;
}
```

```assembly
  .data
  .equ  s_first_name, 0
  .eqw  s_last_name, 30
  .equ  s_class, 60,
  .equ  s_grade, 64,
  .equ  s_size, 68
  
sam: .string "Sam"
smith: .string "Smith"

  sub sp, sp, #s_size
  mov r0, sp
  add r0, r0, #s_first_name
  ldr r1, =sam
  bl  strcpy
  
  mov r0, sp
  add r0, r0, #s_last_name
  ldr r1, =smith
  bl  strcpy
  
  mov r0, sp
  mov r1, #2
  strb r1, [r0, #s_class]
  mov r1, #88
  str r1, [r0, #s_grade]
```
