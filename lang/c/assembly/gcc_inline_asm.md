# GAS Syntax

1. Source-Destination Ordering: `Op-code src dst`

2. register naming: prefixed by `%`

3. immediate operand: `$`; for static "C" variables also prefix a `$`. `0x` for hexadecimal.

4. operand size: the size of memory operands is determined from the last character of the op-code name: `b`, `w`, `l` (32-bit).

5. memory operands: `()`, `[base+index*scale+disp]` in intel syntax = `disp(base, index, scale)` in AT&T syntax.

# Basic Inline

```c
asm("assembly code);
```

```c
// both are equivalent
asm("movl %ecx %eax");
__asm__("movb %bh (%eax)");
```

One than one instructions need `\n` or `\t` in between.

```c
__asm__ ("movl %eax, %ebx\n\t"
         "movl $56, %esi\n\t"
         "movl %ecx, $label(%edx,%ebx,$4)\n\t"
         "movb %ah, (%ebx)");

```

# Extended Asm

```c
asm (assembler template
    : output operands
    : intput operands
    : list of clobbered registers       // theses registers are tampered 
    );
```

- assembler template: each operand is described by an operand-constraint string followed by the C expression in parentheses. Either each instruction is enclosed within double quotes, or the entire group of instructions are within double quotes. Each instruction should end with a delimter (`\n`, `\n\t`, `;`). Operands corresponding to the C expressions are represented by `%0`, `%1` ... etc.

- operands: C expressions serve as operands for the assembly instructions inside `__asm__`. Constraints are primarily used to decide the addressing modes for operands or used in specifying the registers to be used. Each operand is referenced by numbers, counting the first output as `%0`, continuing in increasing order. Output operand expressions must be lvalues. The input operands not restricted and they might be expressions. If the output expression cannot be directly addressed (for example, a bit field), the constraint must allow a register, in which case, GCC will use the register as the output of the asm.

- clobber list: some instructions clobber some hardware registers. Those registers should be listed in the clobber-list. This is to inform GCC that these registers will be modified so that GCC will no assume that the values it loads into these registers will be valid. The input and output should not be listed in the clobber list since they are already specified as contraints.

```c
#include <stdio.h>

int main(int argc, char *argv[])
{
        int a = 10, b;
        __asm__ ("movl %1, %%eax;"
                 "movl %%eax, %0"
                 : "=r"(b)
                 : "r"(a)
                 : "%eax"
                );

        printf("b is now %d\n", b);
        return 0;
}
```

# Reference

https://www.ibiblio.org/gferg/ldp/GCC-Inline-Assembly-HOWTO.html#s1

https://wiki.osdev.org/Inline_Assembly

https://wiki.osdev.org/Inline_Assembly/Examples

https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html

http://cholla.mmto.org/computers/gcc_inline.html
