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

## Useful Tools

1. `ldd`

2. `objdump`, `readelf`: obtain information from an executable file, compiled object or shared library.

3. `nm`: lists the set of symbols defined within an object library or executable program.

## Naming and Versioning

It is possible to create a shared library with a kind of alias called _soname_. Sonames must be used with a symbolic link to the real name of the library.

```shell
gcc -Wl,-soname,libbar.so -o libfoo.so *.o

objdump -p libfoo.so | grep SONAME
```

- _real name_: `libname.so.major.minor`

- _soname_: `libname.so.major-id`, usually created as a relative symbolic link in the directory that contains the real name.

- _linker name_: `libname.so`, usually a symbolic link to the soname is preferred

To solve the library search problem, `ldconfig` creates a cache file of latest minor version of libraries in all the searched directories (`/usr/lib`, `/lib`, `/usr/local/lib` and those specified in `/etc/ld.so.conf`). Also it examines the latest minor version of each major version of each library to find the embedded soname and then creates or updates relative symbolic links for each soname in the same directory. `ldconfig` can also create symbolic links for private libraries (not installed into the standard directories).

### Search Out of the Standard Directories

1. `LD_LIBRARY_PATH`

2. insert into the executable a list of directories that should be searched at runtime for shared libraries using `-rpath` linker options (or set `LD_RUN_PATH` when building an executable if `-rpath` is not specified). Two types of rpaths exists in ELF: `DT_RPATH` and `DT_RUNPATH`. `$ORIGIN` can be set as an argument in rpath to specify the directory of the application, for which the shared libraries will reside in the subdirectory `lib`

### Runtime Search Path

1. `DT_RPATH`

2. `LD_LIBRARY_PATH` for non-setuid and non-setgid programs

3. `DT_RUNPATH`

4. `ld_so.cache`

5. `/lib`, `/usr/lib`

### Runtime Symbol Resolution

1. A definition of a global symbol in the main program overrides a definition in a library.

2. The required global symbol found in the first library searched is used. 

This might cause a library to use a symbol not in its own scope, to avoid this, use `-Bsymbolic` linker option.
