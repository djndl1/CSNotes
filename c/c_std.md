# Introduction

For those library that are defined as functions, C STDLIB permits implementations to provide a function-like macro of the same name in addition to the true function. the macro might provide a faster implementation of a simple function or it might call a function of a different name.

To bypass such macros

```c
double (*p)(double) = &cos; double a = p(b);
double a = (cos)(b);

#undef cos
double a = cos(b);
```

Library headers or file names may be built into the implementation.

Do not use identifiers that begin with an underscore. They are used by an implementation.

Besides the common way, another way to provide compatibility with C++ is 

```cpp
extern "C" {
#include "c_lib.h"
}
```
# Standard Language Additions

Certain Standard C libraries can be considered part of the language. They provide standard definitions and parameterization that help make C programs more portable. They must be provided by freestanding implementations even the other libraries are not provided.

- `stddef.h`: Fundamental types defined by the language, additional basic types and convenience macros. [On why I cannot find stddef.h](https://unix.stackexchange.com/questions/451232/where-is-stddef-h-defined-in-linux)

```c
#ifdef __cplusplus
#define NULL 0L
#else
#define NULL ((void*)0)
#endif

#ifndef __PTRDIFF_TYPE__
#define __PTRDIFF_TYPE__ long int
#endif
typedef __PTRDIFF_TYPE__ ptrdiff_t;

#ifndef __SIZE_TYPE__
#define __SIZE_TYPE__ long unsigned int
#endif
#if !(defined (__GNUG__) && defined (size_t))
typedef __SIZE_TYPE__ size_t;

size_t    // unsigned integer type returned by `sizeof`
ptrdiff_t // signed integer type returned when subtracting two pointers
max_align_t //
offsetof   // byte offset from the beginning of a struct type to specified member
```

- `stdbool.h` C99 supports Boolean arithmetic with the built-in type `_Bool`

```c
#define true 1
#define false 0
#define bool _Bool
```
