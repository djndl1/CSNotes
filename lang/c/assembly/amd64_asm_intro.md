
https://stackoverflow.com/questions/49474166/how-to-write-build-c-code-to-avoid-conflicts-with-existing-assembly-code


https://www.codeproject.com/Articles/45788/The-Real-Protected-Long-mode-assembly-tutorial-for

https://www.agner.org/optimize/

Modern C/C++ compilers do excellent optimization and beginning assembly programmers are no match for a good compiler. It is unlikely for assembly to outperform C/C++ for most general purpose tasks. One advantage of assembly language is that it can do things not possible in high level languages.

> A typical C program has a `main` function which is called indirectly via a `_start` function in the C library.

# GAS Syntax

1. GAS assembly instructions are generally suffixed with the letters `b`, `s`, `w`, `l`, `q` or `t` to determine what size operand is being manipulated. If the suffix is not specified, and there are no memory operands for the instruction, GAS infers the operand size from the size of the destination register operand.

- `s` = single (32-bit floating point)

- `l` = long (32 bit integer or 64-bit floating point)
    
- `q` = quad

- `t` = ten bytes (80-bit floating point)

# Numbers

binary number representation: `1001001010b`

## Floating-point


```
sign bit | exponent | fraction |
```

A number with exponent field equal to 0 is defined to be 0. It is possible to store a negative 0. An exponent of all 1s is used to mean either negative or positive infinity.

# Memory

The hardware mapping registers on an x86-64 CPU can map pages of 2 different sizes - 2MB and 4096 bytes (for kernel and for other cases respectively in Linux). The operation of the memory system is to translate the upper bits of the address from a process's logical address to a physical address.

On Linux, memory for a process is divided into 4 logical regions:

- text: starting at 0 (0x400000 in an x86-64) `.text`

- data: above text `.data`; above data there  is `.bss`, which has uninitialized symbols, thus not stored in the executables.


- heap: dynamically resizable region of memory; also used for loaded shared object

- stack: high address (0x7fffffffffff) (47bits); restricted in size by the Linux kernel, typically to 16 MB. 

```bash
cat /proc/$$/maps
```

## Use `gdb`to examine memory

```bash
p expression
p/FMT expression

x/NFS address // NFS = N Format Size
```

## Memory Mapping

- CR3 (Control Register 3): a pointer to the top level of a hierarchical collection of tables in memory which define the translation from virtual addresses to physical addresses. CR3 is filled with the address of the top level table in the hierarchy (Page Map Level 4). Each process has a unique CR3 value.

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

https://simonis.github.io/Memory/

https://www.kernel.org/doc/html/latest/x86/x86_64/mm.html#complete-virtual-memory-map-with-4-level-page-tables

# Registers

## General-Purpose Registers 

16 general-purpose 64-bit registers:

- `rax` (accumulator)
  
- `rbx` (base register)
  
- `rcx` (count register)
  
- `rdx` (data register)
  
- `rsi` (source index) 
  
- `rdi` (destination index) 

- `rbp` (base pointer)

- `rsp`: (stack pointer)

- `r8`-`r15`

-  `rflags`: flag register. The higher 32 bits are currently reserved and are always read zero.

- `rip`: Instruction pointer

- `CS`, `DS`, `ES`, `FS`, `GS`, `SS`

For most instrucitons, the default operand size in 64-bit mode is 32 bits. 

- To access the full 64-bit operand size, most instructions must contain a REX prefix.



### Legacy Registers

The old 32-bit, 16-bit and 8-bit registers are still available as lower part of the 64-bit registers: `eax`/`ax`/`ah`/`al`, `ebx`/`bx`/`bh`/`bl` ... `r8b`/`r8w`,`r8d`, `eflags`/`flags`, `ip`/`eip`

Operations on the 8-bit and 16-bit parts of the registers do not modify the rest of the register. 32-bit operations use the lower 32-bit part and clear the higher 32-bit.

## Flags Register

The low 16 bits of `rflags` are accessible by application software while the 31:16 of `rflags` contain flags accessible only to system software.

### Control Flag

- `DF`: direction, a flag controls the diretion of string operations.

### Status Flag

Result Information form Logical and Arithmetic Operations and Control Information for conditional move and jump instructions.

- `OF`: overflow

- `SF`: sign

- `ZF`: zero

- `AF`: auxilliary carry

- `PF`: parity

- `CF`: carry

## System Flags

The higher 16 bits

## Floating Point Registers 

- 16 modern floating-point registers (128- or 256-bit)

- 64-bit instruction pointer register `rip` (PC)

- 64-bit flag register `rflags`: currently only 32 bits (`eflags`) are used. The flag register is usually not referred to directly. Conditional instructions are used which internally access  or more flags of the flag register to determine what action to take.

Software can access the 64-bit registers as 64-bit, 32-bit, 6-bit and 8-bit values.

# Operands

## Data Types

- bit 1, nibble 4, byte 8, word 16, doubleword 32, quadword 64, double quadword 128, double octword 256

- unsigned/signed intergers of 8, 16-, 32, 64, 128 bits

- BCD digits 

- half-, single-, double-precision floating point

These fundamental types may be aggregated into composite data types 

- strings of characters, doubleword and quadword: a continuous seqeuence of a single data type.

- packed BCD (two BCD stored in a single byte), packed integers (integer vector), packed single- and double-precision floating point (floating-point vectors)

# Moving Data

Immediate operands can be 1, 2, or 4 bytes for most instructions. 

## Simple Move

- `mov` allows 8 byte immediate values. The source and destination must be the same size and cannot be both memory locations.

```assembly
mov     rax, 100
mov     eax, 100 // shorted, sometimes preferred.
mov   rax, [a]  ; load from memory
```

## Extended Move

- `movzx`/`movz`(GAS): move and zero extend

- `movsx`/`movs`(GAS): move and sign extend

```assembly
movsx   rax, byte [data] ; move byte, sign extend
movzx   rbx, word [sum]  ; move word, zero extend
movsxd  rcx, dword [count] ; move dword, sign extend
```

```assembly
mov   [a], rax  ; move data from rax to a
mov   rbx, rax  ; move data from rax to rbx
```

## Condtional Move

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

## Exchange

- `xchg`: the exchange operation is atomic. This can have a large performance penalty. Only one operand can be in memory, the other must be a register.

- `cmpxchg`: compare and exchange. There is no implicit `lock` prefix.

```c
static inline int a_cas(volatile int *p, int t, int s)
{
	__asm__ __volatile__ (
		"lock ; cmpxchg %3, %1"
		: "=a"(t), "=m"(*p) : "a"(t), "r"(s) : "memory" );
	return t;
}
```

## Array Copy

- `movs` + `b`/`w`/`d`/`p`: moves from the address specified by `rsi` to the address specified by `rdi`. After each data item is moved, `rdi` and `rsi` registers are advanced 1, 2, 4 or 8 bytes depending on the size of the item.

```assembly
// copy 100000 bytes from the array pointed by rsi to the array pointed by rdi
lea     rsi, [source]   ; load effective address
lea     rdi, [destination]
mov     rcx, 100000
rep     movsb
```

## Address Move

- `lea`: Load Effective Address. Calculates the address of `src` and loads it into `dest`. It calculates the address in the same way as `mov`, but it loads the address itself instead of loads the content at that address. One way to abuse `lea` to is to treat content of the register as an address and use `lea` to do multiplication and move `lea rax [rbx+rbx*2]` (multiply `rbx` by three and save the result to `rax`).

## Packed Integer Move

- `movdqa`, `vmovdqa`: move quadword aligned

# Arithmetic

- `neg`: two's complement of its operand, which can be either a general-purpose register or a memory reference; it sets the sign flag (SF) and the zero flag (ZF).

```assembly
neg   rax
neg   dword [x]
neg   byte  [x]
```

- `add`: add the contents of the source to the destination; the source can be an immediate value of 32 bits, a memory reference or a register. Only one of the operands can be a memory reference. `SF` is set to the sign bit of the result; the zero flag `ZF` is set if the result is 0; the overflow flag (OF) is set if the addition overflows. There is no special add for signed numbers versus unsigned numbers.

- `adc`: add with carry

- `inc`: increment

- `sub`: substract the contents of the source from the destination

- `sbb`: subtract with borrow

- `dec`

- `mul`: unsigned integer multiplication

- `imul`: signed integer multiplication

1. `quad` multiplies `rax` by the source (register or memory reference)and stores the result in `rdx`(high):`rax`(low);

2. multiplies the destination by the source

3. multiplies the first source by the second (register or memory) and store in the destination

`CF` and `OF` flags are set when the product exceeds 64 bits, unless a smaller multiply is requested.

- `idiv`/`div`: takes the content in `rdx:rax` as the dividend and takes a single operand (register or memory reference) as the divisor. `rax` for the quotient and `rdx` for the remainder.


# Bit Operations

- `not`: applied to bytes, words, double words, quad-words in registers or in memory

- `and`

- `or`

- `xor`

- `shl`/`shr`/`sal`: shift left/right

- `sar`: propogates the sign bit into the newly vacated positions

To extract a bit field form a word, shift the word right until the right most bit of the field is in the least significant bit position then mask it. To place some bits into position, clear the bits and `or` a mask.

- `rol`/`ror`: rotate left/right 

[Best Practices for Circular Shift Rotate in C](https://stackoverflow.com/questions/776508/best-practices-for-circular-shift-rotate-operations-in-c)

- `bt`: bit test, sets the carry flag to the value of the bit being tested

- `bts`/`btr`: bit test and set/reset

```assembly
; fill in bits 23-51 of sample
mov rax, [sample]
ror rax, 23
shr rax, 29    ; clear the lower 29 bits
shl rax, 29
or rax, [field]
rol rax, 23
mov [sample], rax
```

- `set_` for each of the condition flags in the `eflags` register.

# Branching and Looping

- `jmp`: assembly version of `goto`. `jmp` can jump to an address contained in a register or memory location.

```assembly
segment .data
switch:
  ;; local labels, defined within the range of enclosing regular labels
  dq    main.case0
  dq    main.case1
  dq    main.case2
i: dq    2    ; jump to .case2

  segment .text
  global main

  ;;  a switch clause
main:
  mov   rax, [i]
  jmp   [switch+rax*8]
.case0:
  mov   rbx, 100
  jmp   .end
.case1:
  mov   rbx, 101
  jmp   .end
.case2:
  mov   rbx, 102
.end:
  xor   eax, eax
  ret
```

- conditional jump: the condition codes are based on specific flags in `eflags` such as the zero flag, the sign flag, and the carry flag. `jz`(`je`)/`jnz`(`jne`); `jg`(`jnle`)/`jge`(`jnl`); `jl`(`jngs`, `js`)/`jle`(`jng`); `jc`(`jb`, `jnae`)/`jnc`(`jae`, `jnb`). It is best to stick wit high level coding structures translated to assembly language.

```c
if (a < b) {
  temp = a;
  a = b;
  b = temp;
}
```

```assembly
  mov   rax, [a]
  mov   rbx, [b]
  cmp   rax, rbx
  jge   in_order
  mov   [temp], rax
  mov   [a], rbx
  mov   [b], rax
in_order:
```

```c
if (a < b)
  max = b;
else
  max = a;
```

```assembly
      mov   rax, [a]
      mov   rbx, [b]
      cmp   rax, rbx
      jge   else
      mov   [max], rbx
      jmp   endif
else: mov   [max], rax
endif:
```

```c
if (a < b) 
  result = 1;
else if (a > c)
  result = 2
else 
  result = 3
```

Arbitrary sequence of tests can be used to simulate multiple `else-if` clauses in C:

```assembly
  mov   rax, [a]
  mov   rbx, [b]
  cmp   rax, rbx
  jnl   else_if
  mov   qword [result], 1
  jmp   endif
else_if:
  mov   rcx, [c]
  cmp   rax, rcx
  jng   else
  mov   qword [result], 2
  jmp   endif
else:
  mov   qword [result], 3
endif
```

- while loops

```c
        int sum = 0;
        int i = 0;
        while (i < 64) {
                sum += data & 1;
                data = data >> 1;
                i++;
        }
        return sum;
```

```assembly
  ;;    register usage
  ;;
  ;;    rax: bits being examined
  ;;    rcx: carry bit after bt, setc
  ;;    rdx: sum of one bits
  ;;

  mov   rax, [data]
  xor   ebx, ebx
  xor   ecx, ecx
  xor   edx, edx

while:
  cmp   rcx, 64
  jnl   end_while
  bt    rax, 0
  setc  bl
  add   edx, ebx
  shr   rax, 1
  inc   rcx
  jmp   while
```

```asm
/* 
  a hand-coded supposedly correct bit counts
  that uses `and` instead of `bt` to test the 0th bit
*/
  .text
  .global bit_count_asm
  .type bit_count_asm, @function
bit_count_asm:
  pushq %rbp
  movq  %rsp, %rbp
  subq  $16, %rsp

  xorq  %rax, %rax
  movq  %rdi, 8(%rsp)
.count_start:
  cmp    $0, 8(%rsp)
  jz    .count_end
  
  movq  8(%rsp), %rdi
  andq  $1, %rdi
  addq  %rdi, %rax
  shrq   $1, 8(%rsp)
  jmp   .count_start
.count_end:
  leave
  ret


/* compiler-generated */
bit_count:
        pushq   %rbp
        movq    %rsp, %rbp
        movq    %rdi, -24(%rbp)
        movq    $0, -8(%rbp)
        jmp     .L2
.L3:
        movq    -24(%rbp), %rax
        andl    $1, %eax
        addq    %rax, -8(%rbp)
        shrq    -24(%rbp)
.L2:
        cmpq    $0, -24(%rbp)
        jne     .L3
        movq    -8(%rbp), %rax
        popq    %rbp
        ret
```

```assembly
; GCC generated, better than hand-written

count_ones_bits:
.LFB0:
	.cfi_startproc
	movl	$64, %eax
	xorl	%edx, %edx
	.p2align 4,,10
	.p2align 3
.L2:
	movl	%edi, %ecx
	shrq	%rdi
	andl	$1, %ecx
	addl	%ecx, %edx
	subl	$1, %eax
	jne	.L2
	movslq	%edx, %rax
	ret
	.cfi_endproc

; GCC -funroll-all-loops
count_ones_bits:
.LFB0:
	.cfi_startproc
	movl	$64, %ecx
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L2:
	movl	%edi, %edx
	movq	%rdi, %rsi
	movq	%rdi, %r8
	movq	%rdi, %r9
	andl	$1, %edx
	shrq	%rsi
	movq	%rdi, %r10
	movq	%rdi, %r11
	shrq	$3, %r8
	addl	%edx, %eax
	andl	$1, %esi
	shrq	$4, %r9
	addl	%eax, %esi
	movq	%rdi, %rax
	andl	$1, %r8d
	andl	$1, %r9d
	shrq	$2, %rax
	shrq	$5, %r10
	andl	$1, %eax
	andl	$1, %r10d
	shrq	$6, %r11
	addl	%eax, %esi
	movq	%rdi, %rax
	andl	$1, %r11d
	shrq	$8, %rdi
	addl	%esi, %r8d
	shrq	$7, %rax
	addl	%r9d, %r8d
	andl	$1, %eax
	addl	%r8d, %r10d
	addl	%r11d, %r10d
	addl	%r10d, %eax
	subl	$8, %ecx
	jne	.L2
	cltq
    
	ret
	.cfi_endproc
```

- Do-while loops

```c
        int i = 0;
        char c = data[i];
        if (c != '\0')
                do {
                        if (c == x) break;
                        i++;
                        c = data[i];
                } while (c != '\0');
        return c == 0 ? -1 : i;
```

```assembly
section .data
        data    db      "hello world", 0
        n       dq      0
        needle  db      'w'

section .text
  global main

main:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16

  ;; Register usage
  ;; rax: byte of data array
  ;; rbx: byte to search for
  ;; rcx: loop counter, 0-63
  ;;

        mov     bl, [needle]
        xor     ecx, ecx
        mov     al, [data+rcx]
        cmp     al, 0
        jz      end_while
while:
        cmp     al, bl
        je      found
        inc     rcx
        mov     al, [data+rcx]
        cmp     al, 0
        jnz     while
end_while:
        mov     rcx, -1
found:  mov     [n], rcx
        xor     eax, eax
        ret

```

- counting loops:

```c
        for (int i = 0; i < n; i++)
                c[i] = a[i] + b[i];
```

```assembly
  mov   rdx, [n]
  xor   ecx, ecx
for:
  cmp   rcx, rdx
  je    end_for
  mov   rax, [a+rcx*8]
  add   rax, [b+rcx*8]
  mov   [c+rcx*8], rax
  inc   rcx
  jmp   for
end_for:
```

## Repeat String Instructions

- `rep`: repeats a string/array instruction the number of times specified in `rcx` (count register). The repeat instructions allow setting array elements to a specified value, copying one array to another, and shifting a specific value in an array. The string instructions use `rax` (holding a specific value), `rsi` (source index), `rdi` (destination index). The string operations update `rsi` and `rdi` after each use, managed by `DF` flag (0 for increasing, 1 for decreasing).

- `movsb`: move from one memory location to another (copy an array)

- `stos` + `b`/`w`/`d`/`q`: moves the item in `al`/`ax`/`eax`/`rax` to memory (fill an aray)

```assembly
;; fill an array with 1000000 double words all equal to 1
mov     eax, 1
mov     ecx, 1000000
lea     rdi, [destination]
rep     stosd
```

- `lods` + `b`/`w`/`d`/`q`: moves the item from the address specified by `rsi` to `al`/`ax`/`eax`/`rax`. Better than manually maintaining the counter

```assembly
lea     rsi, [source]
lea     rdi, [destination]
mov     ecx, 1000000
more:   lodsb
        cmp     al, 13
        je      skip
        stosb
skip:   sub     ecx, 1
        jnz     more
```

- `scasb`: searches through an array looking for an item matching the item in `al`

```assembly
segment .text
global strlen
strlen:
    cld     ; clear DF to 0 so that the string operations processes increasing addresses (here `rdi`), the opposite is `std`
    mov     rcx, -1 ; maximum number of iterations, it decreases per repetition
    xor     al, al
    repne   scasb
    mov     rax, -2
    sub     rax, rcx
    ret
```

- `cmpsb`, used with `repe` to compare values until either the count reaches 0 or two different values are located

```assembly
segment . text
global mememp

mememp :
  mov rex, rdx
  repe cmpsb ; compare until end or difference
  cmp rex , 0
  jz equal  ; reached the end
  movzx eax , byte [rdi-1]
  movzx ecx , byte [rsi-1]
  sub rax, rex
  ret
equal:
  xor eax, eax
  ret
```

# Functions

## Stacks

- `push`: subtract 8 from `rsp` and place the value being pushed at that address.

- `pop`: moves the value at the location pointed by `rsp` and then adds 8 to `rsp`.

- `enter`/`leave`: provide support for block structured languages. They establish/release the stack frame on entering/returning from a procedure.

Each function maintains a pointer in `rbp` to a value on the stack identifying the previous value of `rbp` along with the return address. 

```
                          ┌────────────────────────────────┐ lower address
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │       Red Zone                 │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          ├────────────────────────────────┤
rsp  ────────────────────►│  already occupied              │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          │                                │
                          ├────────────────────────────────┤
current rpb   ───────────►│     previous rbp               ├───────────┐
                          ├────────────────────────────────┤           │
                          │   next instruction after ret   │           │
                          ├────────────────────────────────┤           │
                          │                                │           │
                          │                                │           │
                          │                                │           │
                          │                                │           │
                          │                                │           │
                          │                                │           │
                          │                                │           │
                          │                                │           │
                          │                                │           │
                          │                                │           │
                          ├───────────────────────────────┬┘           │
                          │ the 2nd previous rbp          │◄───────────┘
                          ├───────────────────────────────┴┐
                          │ next instruction after ret     │
                          └────────────────────────────────┘ higher address
```


## Call Instruction

- `call`: call a function, the operand is a label in the text segment of a program; it pushes the address of instruction following the call onto the stack and to transfer control to the address associated with the label.

```assembly
;; semantically
  push next_instruction
  jmp  my_function
next_instruction:
```

- `ret`: return from a function; pops the address to return to from the top of the stack and transfer control to that address.

## Calling Convention

- The order in which scalar parameters are allocated

- How parameters are passed

- /register preservation/: which registers the called function must preserve for the caller (_non-volatile_ registers) and which not. /Callee-saved/ registers are guaranteed to retain their values after a subroutine call. Saving here means the caller/callee must save the values in the register somewhere else and restore them later since the registers will be used by other subroutines. 

- how the integrity of the stack is maintained: the arguments pushed on the stack for calling must be cleaned up after returning by the caller or upon returning by the callee.

### [SysV AMD64 Calling Convention](https://raw.githubusercontent.com/wiki/hjl-tools/x86-psABI/x86-64-psABI-draft.pdf)

- The first 6 integer parameters are passed in registers `rdi`, `rsi`, `rdx`, `rcx`, `r8`, `r9`. The first 8 floating point parameters are passed in `xmm0`-`xmm7`. Additional parameters are pushed onto the stack in reverse order. Functions with a variable number of parameters pass the number of floating point parameters in the function call using `rax`. 

- `rdx:rax` for integer return values and `xmm1:xmm0`/`ymm0`/`zmm0` for floating point return values. 

- `r10` for static chain pointer.

- Variadic functions have its number of floating-point arguments passed in by the caller in the `al` register. 

- The stack pointer is expected to be maintained on 16 byte boundaries in memory (ending in `0`) to allow local variables in functions to be placed at 16 byte alignments for SSE and AVX instructions. Executing a `call` would then decrement `rsp`, leaving it ending with an `8`. Conforming functions should either push something or subtract from `rsp` to get it back on a 16 byte boundary.

- `rbx`, `rsp`, `rbp`, `r12`-`r15` are callee-saved. All other registers are caller-saved.

- 128 bytes red zone below the current stack pointer, reserved and safe to use.

```assembly
  section .data
msg:    db      "Hello World!", 0x0a, 0

  section .text
  global main
  extern printf

main:
    ;; establish a stack frame, `leave` before a `ret` undoes it.
  push  rbp
  mov   rbp, rsp

  lea   rdi, [msg]              ; parameter 1 for printf
  xor   eax, eax                ; 0 floating point parameters
  call  printf
  xor   eax, eax                ; return 0

  pop   rbp
  ret
```

`_start` needs all parameters on the stack. `main` is like all other normal C functions.

It's possible to give local variables symbolic names:

```assembly
x   equ     0
y   equ     8

;; save two registers
mov [rsp+x], r8
mov [rsp+y], r9
```

For SysV ABI, registers `rbx`, `rbp` and `r12-15` must be preserved.

```assembly
  segment .data
x     dq      0
scanf_format  db      "%ld",0
printf_format db      "fact(%ld) = %ld",0x0a,0

  segment .text
  global main
  global fact
  extern scanf
  extern printf

main:
  push  rbp
  mov   rbp, rsp

  ;; call scanf
  lea   rdi, [scanf_format]
  lea   rsi, [x]
  xor   eax, eax
  call  scanf

  ;; read input and call factorial
  mov   rdi, [x]
  call  fact

  ;; print the result
  lea   rdi, [printf_format]
  mov   rsi, [x]
  mov   rdx, rax
  xor   eax, eax
  call  printf

  xor   eax, eax
  leave
  ret

fact:
n       equ     8
  push  rbp
  mov   rbp, rsp
  sub   rsp, 16

  cmp   rdi, 1
  jg    greater
  mov   eax, 1
  leave
  ret
greater:
  mov   [rsp+n], rdi
  dec   rdi
  call  fact
  mov   rdi, [rsp+n]
  imul  rax, rdi
  leave
  ret
```

The Linux syscall inteface is different for 32-bit mode and 64-bit mode. Syscalls are defined in `/usr/binclude/asm/unistd_xx.h`. For 32-bit syscalls, place the syscall number in `eax` and use the software interrupt instruction `int 0x80`. Syscalls have parameters which are placed in `ebx`, `ecx`, `edx`, `esi`, `edi` and `ebp`, return values are in `eax`. For 64-bit syscalls, the syscall number is placed in `rax`, the parameters are placed in `rdi`, `rsi`, `rdx`, `r10`, `r8`, `r9`, return values are in `rax`. x86-64 linux uses the `syscall` instruction to execute a syscall.

```assembly
segment .data
msg:    db      "Hello World!",0x0a
len:    equ     $-msg           ; the current assembly point minus the address of msg

  segment .text
  global        main
  extern        write, exit

main:
  push  rbp
  mov   rbp, rsp

  mov   rdx, len
  mov   rsi, msg
  mov   rdi, 1
  call  write
  xor   rdi, rdi
  call  exit

```

## Windows Calling Convention

### Win32

#### cdecl: The Default Convention for x86 C Compilers.

Used by IA-32 [Unix-like](http://sco.com/developers/devspecs/abi386-4.pdf) and Windows.

- Arguments are passed on the stack from right to left. Integer value returned in `eax`, floating point value in `st0`. 

- `eax`, `ecx`, `edx` are volatile and the rest are callee-saved. The x87 FPU registers must be empty when calling and exiting if not used for returning value.

Variations of this conventions exist, leading to incompatibility.

#### stdcall: The Standard Win32 API Convention.

 Callee-cleaned: that is, not only the callee needs to return `rsp` to where it was before the `call`. it also needs to pop out the arguments pushed by the caller by using `ret nbytes_of_arguments`. 

### [Win64 and UEFI](https://docs.microsoft.com/en-us/cpp/build/x64-software-conventions?view=msvc-170)

`stdcall`, `thiscall`, `cdecl`, `fastcall` are all resolved to this convention for 64-bit windows. There is only one AMD64 convention for Win64.

- `rcx`, `rdx`, `r8`, `r9` for integers, `xmm0`, `xmm1`, `xmm2`, `xmm3` for floating point arguments. Additional arguments are pushed onto the stack from right to left.

- Simple structs are passed as if they were integers. Otherwise passed in with a pointer. Oversized struct return value is placed in a caller-provided space, the pointer of which is passed implicitly to the callee as the first argument. 

- `eax`, `xmm0` for return values.

- A 32 bytes on the stack is allocated by the caller for the callee to save the four parameters.
  and later restored by the caller.
  
## Caveats

Structures and classes are not always passed into the callee's stack frame even if the high-level language's semantics seems so. They might be passed by reference.

Structures and classes are probably returned in memory, i.e., an implicit pointer (not necessarily pointing to somewhere in the heap) argument is passed into the callee and populated by the callee and returns the pointer.

## Linux System Calls

### 32 Bit

Put the syscall number in `eax`, parameters in `ebx`, `ecx`, `edx`, `esi`, `edi`, and `ebp` and then `int 0x80`

### 64 Bit

- Syscall number in `rax`

- Parameters in `rdi`, `rsi`, `rdx`, `r10`, `r8`, `r9`. Return values in `rax`

- `syscall`

### C Wrappers

Using C wrappers around syscalls instead of `syscall` is preferred.  

# Arrays

The user should be cautioned not to attempt to assemble programs with large static memory needs on a computer with less RAM than required.


## General Pattern for memory references

- `[label/reg]`

- `[label+2/4/8 *ind]`

- `[reg+k*ind]`

- `[label+reg+k*ind]`

- `[number+reg+k*ind]`

## Array Processing

- creating arrays

```assembly
imul  rdi, 4
call  malloc
```

- fill the array with random numbers. Note that some registers need to be saved on stack across funcall.

```asm
  ;; fill(array, size)

fill:
  ;; local labels
  .array        equ     0 
  .size         equ     8
  .i            equ     16
  push  rbp
  mov   rbp, rsp
  sub   rsp, 32

  mov   [rsp+.array], rdi
  mov   [rsp+.size], rsi
  xor   ecx, ecx
.more   mov     [rsp+.i], rcx
  call  random
  mov   rcx, [rsp+.i]
  mov   rdi, [rsp+.array]
  mov   [rdi+rcx*4], eax
  inc   rcx
  cmp   rcx, [rsp+.size]
  jl    .more
  leave
  ret

```

- printing the array

```asm
  ;; print(array, size)
print:
  .array        equ     0
  .size         equ     8
  .i            equ     16
  push  rbp
  mov   rbp, rsp
  sub   rsp, 32

  mov   [rsp+.array], rdi
  mov   [rsp+.size], rsi
  xor   ecx, ecx
  mov   [rsp+.i], rcx

  segment .data
.format:
  db    "%10d",0x0a,0

  segment .text
.more
  lea   rdi, [.format]
  mov   rdx, [rsp+.array]
  mov   rcx, [rsp+.i]
  mov   esi, [rdx+rcx*4]
  mov   [rsp+.i], rcx
  call  printf
  mov   rcx, [rsp+.i]
  inc   rcx
  mov   [rsp+.i], rcx
  cmp   rcx, [rsp+.size]
  jl    .more
  leave
  ret
```

- finding the minimum

```asm
  ;;  x = min(array, size)

min:
  mov   eax, [rdi]
  mov   rcx, 1
.more
  mov   r8d, [rdi+rcx*4]
  cmp   r8d, eax
  cmovl eax, r8d
  inc   rcx
  cmp   rcx, rsi
  jl    .more
  ret
```

TODO

# Floating point instructions

- `xmm0` ~ `xmm15`: 128-bit; 4 floats or 2 doubles for SSE

- `ymm0` ~ `ymm15`: 256-bit; 8 floats or 4 doubles for AVX

Each `xmm` occupies the first 128 bits of the corresponding `ymm`.

- `movss`, `movsd`: move scalar single/double

- `movaps`/`movapd`: move aligned packed single/double, on a 16-byte boundary in memory

- `movups`/`movupd`: unaligned

- `alignb`: pseudo-op for an array in the bss section

- `align 16`: aligns an array in the data section

Using an aligned move on an unaligned address results in a segfault.

```asm
movups  xmm0, [x]   ; move 4 floats
movups  ymm0, [x]   ; move 8 floats
movupd  [a], xmm15
```

## Arithmetic

- addition: `addss`/`addsd`; the floating point add instrucitons do not set any flags, so testing must be done using a compare instruction. `addps`/`addpd`

```asm
  movss	xmm0, [a]
  addss	xmm0, [b]
  movss	[c], xmm0

  movapd	xmm0, [b]
  addpd	xmm0, [b]
  movapd	[c], xmm0

  movupd	ymm0, [a]
  addpd	ymm0, [b]
  movupd	[c], ymm0
```

- subtraction: `subss`/`subsd`; `subps`/`subpd`

- multiplication: `mulss`/`mulsd`; `mulps`/`mulpd`

- division: `divsd`/`divss`; `divpd`/`divps`

## Conversion

`cvtss2sd`, one float to double; `cvtps2pd`: two packed floats to 2 packed doubles; `cvtsd2ss`/`cvtpd2ps`; `cvtss2si`: float to quad-word integer or a double, `cvtsd2si`: double to a double or a quad-word integer. `cvttss2si`/`cvttsd2si`: truncate and convert. `cvtsi2ss`/`cvtsi2sd`: convert a quad-word integer to a single/double. When using a memory location, `dword` or `qword` may be added to specify the size.

## Comparison

- quiet NaN and signalling NaN

- ordered comparison and unordered comparison

- GCC uses unordered comparisons where only SNaN causes an exception.

- `ucomiss`/`ucomisd`

- `jb`, `jbe`, `ja`, `jae`

## Mathematical Functions

8087 mathemtical functions are deprecated in favor of library functions. SSE instructions contain some other common floating-point function.

- max/min: `minss`/`maxss`; `minsd`/`maxsd`; `minps`/`minpd`; `maxps`/`maxpd`

```asm
  movss xmm0, [x]
  maxss xmm0, [y]

  movapd        xmm0, [a]
  minpd         xmm0, [b]
```

- rounding: `roundss`/`roundps`; `roundsd`/`roundpd`: rounding floating point numbers to whole numbers. A third operand is for rounding mode selection.

- square roots: `sqrtss`/`sqrtsd`; `sqrtps`/`sqrtpd`

$$
d=\sqrt{\left(\left(x_{1}-x_{2}\right)^{2}+\left(y_{1}-y_{2}\right)^{2}+\left(z_{1}-z_{2}\right)^{2}\right)}
$$

```asm
distance3d:
  push  rbp
  mov   rbp, rsp

  movss xmm0, [rdi]
  subss xmm0, [rsi]
  mulss xmm0, xmm0
  movss xmm1, [rdi+4]
  subss xmm1, [rsi+4]
  mulss xmm1, xmm1
  movss xmm2, [rdi+8]
  subss xmm2, [rsi+8]
  mulss xmm2, xmm2
  addss xmm0, xmm1
  addss xmm0, xmm2
  sqrtss xmm0, xmm0
  leave
  ret
```

$$
d=x_{1}x_{2}+y_{1}y_{2}+z_{1}z_{2}
$$

```asm
dot_product:
  push  rbp
  mov   rbp, rsp

  movss xmm0¸ [rdi]
  mulss xmm0, [rsi]
  movss xmm1, [rdi+4]
  mulss xmm1, [rsi+4]
  movss xmm2, [rdi+8]
  mulss xmm2, [rsi+8]
  addss xmm0, xmm1
  addss xmm0, xmm2

  leave
  ret
```

Polynomial evalutation

$$
P\left(x\right)=p_{0}+p_{1}x+p_{2}x^{2}+\cdots+p_{n}x^{n}
$$`

```asm
polynomial:
  ;; rdi: array of double coefficients
  ;; xmm0: x
  ;; rsi: degree

  push  rbp
  mov   rbp, rsp
.horner:
  movsd xmm1, xmm0              ; xmm1 for x
  movsd xmm0, [rdi+8*rsi]       ; b_k
  cmp   rsi, 0
  jz    .done
.more:
  sub   rsi, 1
  mulsd xmm0, xmm1
  addsd xmm0, [rdi+8*rsi]
  jnz   .more
.done:
  leave
  ret
```

# Structs

```asm
  struc Customer
.id      resd    1
.name    resb    64
.address resb    64
.balance resd    1
  endstruc
```

Now it is possible to refer to the offset using `Customer.id`.

```asm
  segment .data
name    db      "Calvin",0
address db      "12 Mockingbird Lane", 0
balance dd      12500
c       dq      0
  struc Customer
.id             resd    1
.name           resb    64
.address        resb    64
.balance        resd    1
  endstruc
  segment .text
  global main
  extern malloc, strcpy

main:
  push  rbp
  mov   rbp, rsp
  sub   rsp, 32

  mov   rdi, Customer_size      ; defined by the assembler
  call  malloc
  mov   [c], rax
  mov   [rax+Customer.id], dword 7
  lea   rdi, [rax+Customer.name]
  lea   rsi, [name]
  call  strcpy
  mov   rax, [c]
  lea   rdi, [rax+Customer.address]
  lea   rsi, [address]
  call  strcpy
  mov   rax, [c]
  mov   rdx, [balance]
  mov   [rax+Customer.balance], edx
  xor   eax, eax
  leave
  ret
```
