# Unix System Overview

All OSes provide services for programs they run, including executing a new programs, opening a file, reading a file, allocating a region of memory, getting the current time of day.

Libraries of common functions are built on top of the system call, but applicaitons are free to use both.

The Bourne-again shell is the GNU shell provided with all Linux systems.

The only two characters that cannot appear in a filename are the slash `/` and the null character.

All threads within a process share the same address space, file descriptors, stacks, and process-related attributes. In an environment that supports threads, each thread needs its own local copy of `errno` to prevent one thread from interfering with another.

The errors can be divided into two categories: _fatal_ (non-recoverable) and _nonfatal_ (can be dealt with more robustly).

Signals are a technique used to notify a process that some condition has occurred. The process can choose to ignore the signal, let the default action occur or provide a function that is called when the signal occurs (catch the signal).

 Unix maintains _calendar time_, counting from the Epoch and _process. UNIX maintains _clock time_, _user CPU time_ and _system CPU time_ for a process.
 
# Standardization and Implementation 


## ISO C

C99 largely improved support for applications that perform numerical processing. The changes don’t affect the POSIX interfaces described in this book, except for the addition of the `restrict` keyword to some of the function prototypes.

## IEEE POSIX (Portable Operating System Interface)


- Originally IEEE 1003.1-1998, later 1003.2 added. 1003.1 mainly specifies the OS interface. No distinction is made between syscalls and library functions. All theroutines in the standard are called functions.

- IEEE 1003.1-1990 _POSIX.1_

- IEEE 1003.1b-1993 real-time extension standard, the interfaces for multthreading programming, called pthreads.

- IEEE 1003.1j-2000, IEEE 1003.1q-2000: more real-time extension and event-tracing extension

- IEEE 1003.1-2001

- POSIX.1 2008: IEEE 1003.1, 2004; Open Group Technical Standard 2006; ISO/IEC 9899;1999

The interfaces are divided into required one and optional ones, which are further divided into 4 secions based on functionality. Many options deal with real-time extensions.

The Single UNIX Specification, a superset of the POSIX.1 standard, specifies additional intefaces that extend the functionality provided by the POSIX.1 specification. POSIX.1 is equivalent to the Base Specification portion of the Single UNIX Specification. The X/Open System Interfaces (XSI) option in POSIX.1 describes optional interfaces and defines which optional portions of POSIX.1 must be supproted for an implementation to be deemded XSI conforming. Only XSI-conforming implementation can be called UNIX systems.

## Limits, features

_Compile-time limits_ (e.g. the largest value of a short integer, defined in headers) and _runtime limits_ (how many bytes in a filename, requires a function call). 

- compile time limits (headers)

- runtime limits not associated with a file or directory (`sysconf`)

- runtime limits that are associated with a file or directory (`pathconf`, `fpathconf`)

All of the compile-time limits defined by ISO C are `<limits.h>`. POSIX has its own requirements on that.

POSIX.1 defines numerous constants that deal with implementation limits of the OS. `_POSIX*` is the minumum acceptable value for maximum. Constants with `_POSIX` is the actual value. Not all of them are guaranteed to be defined inside `<limits.h>`. Some constants have to be retrieved at runtime, using `sysconf`, `fpathconf` or `pathconf`. These three functions are actuall what should be used to find out about the above limits.

`sysconf` accepts arguments starting with `_SC_` and the latter two accept `_PC_`.

XSI can also determined using the three functions, whose arguments are prefixed with `_SC_XOPEN` or `_PC_OPEN`.

Macro `_POSIX_C_SOURCE` excludes any implementation-defined definitions. `_XOPEN_SOURCE` enables XSI options.

## Primitive data types

Certain C data types have been associated with certain UNIX system variables. The header `<sys/types.h>` defines some implementation-dependent data types, most ending in `_t`.
