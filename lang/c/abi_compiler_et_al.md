# Libraries

http://nickdesaulniers.github.io/blog/2016/08/13/object-files-and-symbols/

http://nickdesaulniers.github.io/blog/2016/11/20/static-and-dynamic-libraries/

http://tldp.org/HOWTO/Program-Library-HOWTO/index.html

http://nickdesaulniers.github.io/blog/2015/05/25/interpreter-compiler-jit/

http://nickdesaulniers.github.io/blog/2013/04/03/basic-jit/

http://nickdesaulniers.github.io/blog/2014/04/18/lets-write-some-x86-64/


```bash
ar -r # for creating static libraries
```

```bash
cc -shared -fPIC obj1.o obj2.o ... -o libso.so
```

```bash
strace    # observe dynamic linker in action
ldd
readelf -d
```

- `LD_LIBRARY_PATH` env var 

- `LD_DEBUG`

- `LD_PRELOAD`: inject a custom shared library to be linked instead of the expected library.

# LLDB & GDB




# `libgcc`

https://wiki.osdev.org/Libgcc

https://gcc.gnu.org/onlinedocs/gccint/Libgcc.html

Shared code that would be inefficient to duplicate every time as well as auxilliary routines and runtime support. It's included by default when linking with GCC.
