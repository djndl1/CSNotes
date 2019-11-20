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
