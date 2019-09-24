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
ptrdiff_t // signed integer type returned when subtracting two 
pointers
max_align_t //
offsetof   // byte offset from the beginning of a struct type to specified member
```

- `stdbool.h` C99 supports Boolean arithmetic with the built-in type `_Bool`

```c
#define true 1
#define false 0
#define bool _Bool
```

- `iso646.h`

```c
#ifndef _ISO646_H
#define _ISO646_H

#ifndef __cplusplus // in C++, these identifiers are keywords

#define and    &&
#define and_eq &=
#define bitand &
#define bitor  |
#define compl  ~
#define not    !
#define not_eq !=
#define or     ||
#define or_eq  |=
#define xor    ^
#define xor_eq ^=

#endif

#endif

```

- `errno.h`: error report 

```c
#define	EPERM		 1	/* Operation not permitted */
#define	ENOENT		 2	/* No such file or directory */
#define	ESRCH		 3	/* No such process */
#define	EINTR		 4	/* Interrupted system call */
#define	EIO		 5	/* I/O error */
#define	ENXIO		 6	/* No such device or address */
#define	E2BIG		 7	/* Argument list too long */
#define	ENOEXEC		 8	/* Exec format error */
#define	EBADF		 9	/* Bad file number */
#define	ECHILD		10	/* No child processes */
#define	EAGAIN		11	/* Try again */
#define	ENOMEM		12	/* Out of memory */
#define	EACCES		13	/* Permission denied */
#define	EFAULT		14	/* Bad address */
#define	ENOTBLK		15	/* Block device required */
#define	EBUSY		16	/* Device or resource busy */
#define	EEXIST		17	/* File exists */
#define	EXDEV		18	/* Cross-device link */
#define	ENODEV		19	/* No such device */
#define	ENOTDIR		20	/* Not a directory */
#define	EISDIR		21	/* Is a directory */
#define	EINVAL		22	/* Invalid argument */
#define	ENFILE		23	/* File table overflow */
#define	EMFILE		24	/* Too many open files */
#define	ENOTTY		25	/* Not a typewriter */
#define	ETXTBSY		26	/* Text file busy */
#define	EFBIG		27	/* File too large */
#define	ENOSPC		28	/* No space left on device */
#define	ESPIPE		29	/* Illegal seek */
#define	EROFS		30	/* Read-only file system */
#define	EMLINK		31	/* Too many links */
#define	EPIPE		32	/* Broken pipe */
#define	EDOM		33	/* Math argument out of domain of func */
#define	ERANGE		34	/* Math result not representable */

...
```

An thread-local `int ``errno` holds implementation defined error codes from library routine and is never cleared by library routines.

```c
/* The error code set by various library functions.  */
extern int *__errno_location (void) __THROW __attribute_const__;
# define errno (*__errno_location ())

```

The typical way of using `errno` is to clear it before calling a library function and check it afterwards. `strerror`, `perror` interpretes `errno`.

- `stdarg.h`: a portable way to access variable argument lists

```c
TYPEDEF __builtin_va_list va_list;

#define va_start(v,l)   __builtin_va_start(v,l) // initializes a va_list
#define va_end(v)       __builtin_va_end(v)     // called after all the argument have been read

#define va_arg(v,l)     __builtin_va_arg(v,l)  // returns the value of the next parameter in the argument
#define va_copy(d,s)    __builtin_va_copy(d,s) // duplicates the current state of a va_list
```

`va_copy` can be used to retain a pointer into the argument list

# Character Processing `<ctype.h>` `<wctype.h>`

There are two kinds of facilities for handling characters: classification (`is*`)and conversion (`to*`). Every character conversion facility has a name beginning with `to` and returns a value of type `int` representing a character or `EOF`. `EOF` is not necessarily distinguishable form a real character if nonstandard character values appear (Standard character values are always non-negative even if the type `char` is signed).

The formulation of the facilities in C STDLIB takes into account the possibility that several locales will be supported.

- `isalnum`, `isalpha`, `iscntrl` and their wide character counterparts, `isascii` (a BSD and SVID extension).

```c
bool is_legal_cid(const char *s)
{
    char ch;
    if ((ch = *s++) == '\0') return true;
    if (!(isalpha(ch) || ch == '_')) return false;
    while ((ch = *s++) != '\0') {
        if (!(isalpha(ch) || ch == '_')) return false;
    }
    
    return true;
}
```

- `isdigit`, `isxdigit` and their wide character counterparts

- `isgraph`, `ispunct`, `isprint` (any character that is not a control character) and their wide character counterparts. A space is a printing character. `isprint` and `isgraph` differ only in how they handle the space character.

- `islower`, `isupper` and their wide character counterparts

- `isblank`, `isspace` and their wide character counterparts. Space characters are `\t`, `\v`, `\r`, `\n`, `\f`, `' '`. Blank characters are characters used to separate words within a line of text, including standard blank characters, space and horizontal tab, and they may include additional locale-specific characters for which `isspace` is true.


- `tolower`, `toupper` and their wide character counterparts

# String Processing `string.h`, `stdlib.h`

- `strcat` `strncat` (n characters ('\0' excluded)) and their wide character counterparts: semantically, `dest = dest + src`;

- `strcmp`, `strncmp`

- `strcpy`, `strncpy` (it may add additional '\0's )

```c
char *strcat(char *dest, const char  *src)
{
    char *s = dest + strlen(dest);
    strcpy(s, src);
    return dest;
}
```

- `strlen`

- `strchr`, `strrchr`: finds the first/last occurrence of a character

```c
int how_many(const char *s, int c)
{
    int n = 0;
    if (c == 0) return 0;
    while (s) {
        s = strchr(s, c);
        if (s) n++, s++;
    }
    
    return n;
}
``

- `strspn`, `strcspn`, `strpbrk`: search a null terminated string for occurrences of characters specified by whether they are included in a second NTBS.

```c
#include <stdbool>
#include <stddef.h>
#include <string.h>

int is_id(const char *s)
{
        static char *id_chars =
                "abcdefghijklmnopqrstuvwxyz"
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                "0123456789_";
        if (s == NULL) return false;
        if (strspn(s, id_chars) != strlen(s)) return false;

        return !isdigit(*s);
}
```

- `strtok` (finds the next token), `strstr`: C++ `string.find()`

```c
        char input[] = "A bird   came down the walk";
        printf("Parsing the input string '%s'\n", input);
        char *token = strtok(input, " ");
        while(token) {
                puts(token);
                token = strtok(NULL, " ");
        }
```

- `strcoll, strxfrm`: provide locale-specific string-sorting facilities.

- `atof`, `atoi`, `atol`, `atoll`: convert the initial portion of a string to numbers

- `strtod`, `strtof`, `strtold`; `strtol`, `strtoll`, `strtoul`, `strtoull`: These functions provide more control over conversions than the corresponding facilities of `sscanf`.

# Memory Functions

In STD C, memory is interpreted as an array of objects of type `unsigned char`. Block of memory are designated by a pointer of type `void *`.

- `memchr`: searches for the first occurrence of a value in the first `len` characters beginning at a certain place

- `memcmp`: compare the first few bytes of two memory block

- `memcpy`, `memmove`: the difference is that `memmove` will work correctly for overlapping memory regions.

- `memset`: copies a value into a certain number of  bytes beginning at a memory location.
