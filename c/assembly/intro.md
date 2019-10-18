Modern C/C++ compilers do excellent optimization and beginning assembly programmers are no match for a good compiler. It is unlikely for assembly to outperform C/C++ for most general purpose tasks. One advantage of assembly language is that it can do things not possible in high level languages.

> A typical C program has a `main` function which is called indirectly via a `_start` function in the C library.

# Numbers

binary number representation: `1001001010b`

## Floating-point

```
sign bit | exponent | fraction |
```

A number with exponent field equal to 0 is defined to be 0. It is possible to store a negative 0. An exponent of all 1s is used to mean either negative or positive infinity.
