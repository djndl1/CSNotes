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
