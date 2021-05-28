# Static Libraries

Created and maintained by `ar(1)`. Object files are stored into the archive in a structure that makes it possible to retrieve the original invidual files, thus the members can be listed, replaced and deleted.

`ar` is used with an operation mode (deletion, replace etc.) and a number of modifiers to specify variations on an operation's behavior.

# Shared Libraries

## Overview

Some details:

1. Code sections are shared between processes, data sections are not.

## Creation

```shell
gcc -c -fPIC csource.c # position-independent code
gcc -shared -o libfoo.so csource.o # use -shared
```

Position-independent code changes the way the compiler generates code for operations such as accessing global, static and external variables; access string constants; and taking the address of functions, which allows the code to be located at any virtual address at run time.

## Using

1. The name of the shared library is embedded in the executable, in its _dynamic dependency list_.

2. A _dynamic linker_/_dynamic linking loader_/_runtime linker_ is required to link to the shared library at runtime, which itself is a shared library. Search paths are predefined but also controlled by some envvars (`LD_LIBRARY_PATH` searched before the standard path).

Every program—including those that use shared libraries—goes through a static-linking phase.
