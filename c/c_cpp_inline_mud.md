# `inline` in C99 is basically about linkage so that `inline` function is used only for inlining

Since C99, a function declaration with no declared linkage does not generate a function object. The inline definition will only be used with inline substitution, and the compiler is not obliged to perform this optimization. It is expected that an external definition of the function exists in some other translation unit, and such a definition must exist if the function object is used, either by taking its address or by being called in a context where the compiler chooses not to perform the inline substitution. If the inline function is declared with either static or extern, then a function object is compiled, with the indicated linkage, thereby satisfying the requirement that the function object be defined.

The C99 standard says

> Any function with internal linkage can be an inline function. For a function with external linkage, the following restrictions apply: If a function is declared with an inline function specifier, then it shall also be defined in the same translation unit.

> If all of the file scope declarations for a function in a translation unit include the inline function specifier without extern, then the definition in that translation unit is an inline definition. An inline definition does not provide an external definition for the function, and does not forbid an external definition in another translation unit. An inline definition provides an alternative to an external definition, which a translator may use to implement any call to the function in the same translation unit. It is unspecified whether a call to the function uses the inline definition or the external definition.

Note the terms _inline definition_, _external definition_ and the phrasing of "_alternative to an external definition_".

```c
// inline_test_a.c
#include <stdio.h>

inline int foo()
{
    return 3;
}

void g()
{
    printf("foo called from g: return value = %d, address = %#p\n", foo(), &foo);
}
```

```c
// inline_test_b.c
#include <stdio.h>

extern inline int foo()
{
    return 4;
}

void g();

int main()
{
    printf("foo called from main: return value = %d, address = %p\n", foo(), &foo);
    g();
    return 0;
}
```
Compile these two without optimisation using `clang`

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  clang -c inline_test_a.c                                                                                                                                                                  
inline_test_a.c:10:62: warning: flag '#' results in undefined behavior with 'p' conversion specifier [-Wformat]
    printf("foo called from g: return value = %d, address = %#p\n", foo(), &foo);
                                                            ~^~
1 warning generated.

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  clang -c inline_test_b.c 
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  clang inline_test_a.o inline_test_b.o -o inline_test
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 4, address = 0x401170
foo called from g: return value = 4, address = 0x401170
  
```

The `inline` version `foo()` which should return 3 is not used at all, didn't have external linkage.

However, with optimization option the `inline` `foo()` is used but still without external linkage. The address printed is the address of the `extern inline` `foo()`  which has external linkage.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  clang -c inline_test_a.c -O2                                                                                                                                                          
inline_test_a.c:10:62: warning: flag '#' results in undefined behavior with 'p' conversion specifier [-Wformat]
    printf("foo called from g: return value = %d, address = %#p\n", foo(), &foo);
                                                            ~^~
1 warning generated.

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  clang inline_test_a.o inline_test_b.o -o inline_test

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 4, address = 0x401150
foo called from g: return value = 3, address = 0x401150
```

if we try to read the symbol table:

```bash
 djn-pc  djn-pc-lenovo  ../CodeSpace/playground  gcc -c inline_test_a.c

 djn-pc  djn-pc-lenovo  ../CodeSpace/playground  readelf -s inline_test_a.o 

Symbol table '.symtab' contains 13 entries:
   Num:    Value          Size Type    Bind   Vis      Ndx Name
     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND 
     1: 0000000000000000     0 FILE    LOCAL  DEFAULT  ABS inline_test_a.c
     2: 0000000000000000     0 SECTION LOCAL  DEFAULT    1 
     3: 0000000000000000     0 SECTION LOCAL  DEFAULT    3 
     4: 0000000000000000     0 SECTION LOCAL  DEFAULT    4 
     5: 0000000000000000     0 SECTION LOCAL  DEFAULT    5 
     6: 0000000000000000     0 SECTION LOCAL  DEFAULT    7 
     7: 0000000000000000     0 SECTION LOCAL  DEFAULT    8 
     8: 0000000000000000     0 SECTION LOCAL  DEFAULT    6 
     9: 0000000000000000    48 FUNC    GLOBAL DEFAULT    1 g
    10: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND _GLOBAL_OFFSET_TABLE_
    11: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND foo
    12: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND printf
```

Strangely, pure `inline` actually emitted a symbol however it's undefined at where `foo()` is defined. It's reasonable if we think about what the standard says about the "alternative" stuff. `inline` is like an `extern` declaration of an external definition.

```c
// inline_test_a.c
#include <stdio.h>

int foo()
{
    return 3;
}

// inline_test_b.c
#include <stdio.h>

inline int foo()
{
    return 4;
}

int main()
{
    printf("foo called from main: return value = %d, address = %p\n", foo(), &foo);
    return 0;
}
```

```bash
djn-pc  djn-pc-lenovo  ../CodeSpace/playground  ./inline_test
foo called from main: return value = 3, address = 0x5567dadd2169
```

Note we didn't declare `extern int foo()` in `inline_test_b.c` but we were able to call the external `foo()` in `inline_test_a.c`. Exactly what the rationale behind the C99 `inline`.

[Myth and reality about inline in C99]https://gustedt.wordpress.com/2010/11/29/myth-and-reality-about-inline-in-c99/

C99 `inline` attempted to implement the same idea as C++ did:

1. have the definition of a function in a header file, visible to the whole project;

2. have only one external symbol generated, if the function can’t be inlined at some place;

To do this, put the inline definition in a header and put `extern inline typename abc` in one of the TU where `abc` is used to enforce ODR (C++ does it by discarding duplicate definition. The C99 way is actually cumbersome since only one `extern inline` declaration should be present). `static inline` just bloats the whole project.


# GCC GNU89 extension makes `inline` keyword give default external linkage

Without modifying the source, recompiling with gnu89 inline extension actually make the `inline` `foo()` have external linkage so that `main` function can return 3.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -c inline_test_a.c inline_test_b.c -std=gnu89

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc inline_test_a.c inline_test_b.o -o inline_test -std=gnu89                                                                                                                             

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 3, address = 0x55c34cfc9135
foo called from g: return value = 3, address = 0x55c34cfc9135
```

adding `-O2` optimization makes things a little different, `extern inline` `foo()` is actually inlined inside `main`.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -c inline_test_a.c inline_test_b.c -std=gnu89 -O2

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc inline_test_a.c inline_test_b.o -o inline_test -std=gnu89 -O2

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 4, address = 0x55c5ad6cd170
foo called from g: return value = 3, address = 0x55c5ad6cd170
```

Again, optimizing only the `inline` `foo()`, tells us that it really has external linkage, otherwise, `main` wouldn't be able to print 3.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -c inline_test_a.c -std=gnu89 -O2

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -c inline_test_b.c -std=gnu89

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc inline_test_a.c inline_test_b.o -o inline_test -std=gnu89

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 3, address = 0x564152caa135
foo called from g: return value = 3, address = 0x564152caa135
```

While optimizing only `extern inline foo()` inlines.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -c inline_test_a.c -std=gnu89

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -c inline_test_b.c -std=gnu89 -O2

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc inline_test_a.c inline_test_b.o -o inline_test -std=gnu89 

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 4, address = 0x557488ca1165
foo called from g: return value = 3, address = 0x557488ca1165
```

Modify the `extern inline` `foo()` to `static inline` and recompile. See how two functions both got called and have linkage. `g()` uses the `foo()` defined in `inline_test_a.c` and `main` uses the `static inline foo()`. 

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -c inline_test_a.c inline_test_b.c -std=gnu89

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc inline_test_a.c inline_test_b.o -o inline_test -std=gnu89                                                                                                                             

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 4, address = 0x55a11d57c16b
foo called from g: return value = 3, address = 0x55a11d57c135
```

Finally, make both functions `inline` only. This doesn't even compile since there are two `foo()` with external linkage, causing conflicting definitions.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -c inline_test_a.c inline_test_b.c -std=gnu89

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc inline_test_a.c inline_test_b.o -o inline_test -std=gnu89 
/usr/bin/ld: inline_test_b.o: in function `foo':
inline_test_b.c:(.text+0x0): multiple definition of `foo'; /tmp/ccIuseGz.o:inline_test_a.c:(.text+0x0): first defined here
collect2: error: ld returned 1 exit status
```

GCC documentation clearly states

> When an inline function is not static, then the compiler must assume that there may be calls from other source files; since a global symbol can be defined only once in any program, the function must not be defined in the other source files, so the calls therein cannot be integrated. Therefore, a non-static inline function is always compiled on its own in the usual fashion.

> If you specify both inline and extern in the function definition, then the definition is used only for inlining. In no case is the function compiled on its own, not even if you refer to its address explicitly. Such an address becomes an external reference, as if you had only declared the function, and had not defined it.

Finally, make both `extern inline` and compile with `-std=gnu89` gives:

```c
inline_test_a.c:(.text+0xa): undefined reference to `foo'
/usr/bin/ld: inline_test_a.c:(.text+0x13): undefined reference to `foo'
/usr/bin/ld: /tmp/cceqRird.o: in function `main':
inline_test_b.c:(.text+0xa): undefined reference to `foo'
/usr/bin/ld: inline_test_b.c:(.text+0x13): undefined reference to `foo'
collect2: error: ld returned 1 exit status
```

which means no linkage for `extern inline` in GNU C89.

Fortunately, GCC now defaults to ISO C99 inline semantics unless `-fgnu89-inline` is explicitly specified.

If `__attribute__((always_inline))` is used before `inline` `foo()`, then it is always inlined. Clang also support this attribute.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  gcc -o inline_test inline_test_a.c inline_test_b.c

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 4, address = 0x559d454f115e
foo called from g: return value = 3, address = 0x559d454f115e
```

`__inline__` is a nonstandard keyword and used only before C99 since C89 disables `inline` and is compatible with both GCC and clang (`__inline` works too!).

# How about C++

In C++, `inline` are treated by default as having external linkage and the program behaves as if there is only one copy of the function. Redefining an inline function with the same name but with different function body is illegal; however, the compiler does not flag this as an error, but simply generates a function body for the version defined in the first file entered on the compilation line and discards the others. 

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  g++ inline_test_b.cc inline_test_a.cc -o inline_test

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 4, address = 0x56378ac0a164
foo called from g: return value = 4, address = 0x56378ac0a164

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  g++ inline_test_a.cc inline_test_b.cc -o inline_test                                                                                                                                      

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 3, address = 0x555cdb23b15b
foo called from g: return value = 3, address = 0x555cdb23b15b
```

Commenting out `foo()` in `inline_test_b.c` and replacing it with `extern int foo()`, it still compiles:

```bash
foo called from main: return value = 3, address = 0x55f8a9a3615b
foo called from g: return value = 3, address = 0x55f8a9a3615b
```

meaning that `inline` in C++ does have external linkage. 

`extern inline` has the same semantics as `inline`

Removing one of the `inline` specifiers, the test program still compiles and uses the `inline`d one, which shows the external linkage of `inline`.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  g++ inline_test_b.cc inline_test_a.cc -o inline_test

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 3, address = 0x5594578ff18f
foo called from g: return value = 3, address = 0x5594578ff18f
```

However, this is not the whole story!

> The definition of an inline function or variable must be reachable in the translation unit where it is accessed (not necessarily before the point of access).

[inline functions](https://isocpp.org/wiki/faq/inline-functions)
[inline specifier](https://en.cppreference.com/w/cpp/language/inline)

 An inline function must have its definition in the same translation unit as the caller.

```cpp
// inline_test_c.hh
class A {
public:
    void foo();
};

// inline_test_c.cc
#include "inline_test_c.hh"

#include <cstdio>

inline void A::foo()
{
    printf("3\n");
}
```

```cpp
// inline_test_d.cc
#include "inline_test_c.hh"

int main(int argc, char *argv[])
{
    A a{};
    a.foo();
    return 0;
}
```

```bash
 djn-pc  djn-pc-lenovo  ../CodeSpace/playground  g++ inline_test_d.cc inline_test_c.cc -o inline_test_2
/usr/bin/ld: /tmp/ccbz2Io5.o: in function `main':
inline_test_d.cc:(.text+0x17): undefined reference to `A::foo()'
collect2: error: ld returned 1 exit status
```

Wait, this seems to contradict the statement that C++ inline function has external linkage. However

```cpp
//inline_test_a.cc

#include <stdio.h>

inline int foo()
{
    return 3;
}

//inline_test_b.cc
#include <stdio.h>

extern int foo();

int main()
{
    printf("foo called from main: return value = %d, address = %p\n", foo(), &foo);
    return 0;
}
```

```bash
 djn-pc  djn-pc-lenovo  ../CodeSpace/playground  g++ inline_test_a.cc inline_test_b.cc -o inline_test
/usr/bin/ld: /tmp/ccCEaGuZ.o: in function `main':
inline_test_b.cc:(.text+0x5): undefined reference to `foo()'
/usr/bin/ld: inline_test_b.cc:(.text+0xe): undefined reference to `foo()'
collect2: error: ld returned 1 exit status
```

In summary, about why previously an `inline` function was able to call outside where it was defined without including its definition, the rule for the external linkage about C++ `inline` should be rephrased as: inline function has external linkage only when it is called in the same translation unit somewhere, otherwise, it has no linkage at all. C++ got the whole thing somewhat right: `inline` is meant for inline expansion, so it must be in the same TU as the caller. If it's not called, then it shouldn't be used otherwise (like called as an external function). It has external linkage so that only one copy is reversed to prevent multiple duplicates with internal linkage. Nevertheless, C99 inline makes no assumption on where and if an external symbol for an inline function is emitted and if gives you a syntax to explicitly require emission in a compilation unit of your liking.

To prove this, add 

```cpp
void g()
{
    A a{};
    a.foo();
}
```

to `inline_test_c.cc` where `inline void A::foo()` is defined, now `main` inside `inline_test_d.cc` can call `A::foo()`. If we look at the symbol table of `inline_test_c.o`

```
 djn-pc  djn-pc-lenovo  ../CodeSpace/playground  readelf -s inline_test_c.o 

Symbol table '.symtab' contains 15 entries:
   Num:    Value          Size Type    Bind   Vis      Ndx Name
     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND 
     1: 0000000000000000     0 FILE    LOCAL  DEFAULT  ABS inline_test_c.cc
     2: 0000000000000000     0 SECTION LOCAL  DEFAULT    2 
     3: 0000000000000000     0 SECTION LOCAL  DEFAULT    4 
     4: 0000000000000000     0 SECTION LOCAL  DEFAULT    5 
     5: 0000000000000000     0 SECTION LOCAL  DEFAULT    6 
     6: 0000000000000000     0 SECTION LOCAL  DEFAULT    7 
     7: 0000000000000000     0 SECTION LOCAL  DEFAULT   10 
     8: 0000000000000000     0 SECTION LOCAL  DEFAULT   11 
     9: 0000000000000000     0 SECTION LOCAL  DEFAULT    9 
    10: 0000000000000000     0 SECTION LOCAL  DEFAULT    1 
    11: 0000000000000000    27 FUNC    WEAK   DEFAULT    7 _ZN1A3fooEv
    12: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND _GLOBAL_OFFSET_TABLE_
    13: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND puts
    14: 0000000000000000    23 FUNC    GLOBAL DEFAULT    2 _Z1gv
```

There is a `_ZN1A3fooEv` with a weak binding and is defined, which is the same case as with the unmodified `inline_test_a.cc`. Inside `inline_test_d.o`, it's always undefined

```cpp
Symbol table '.symtab' contains 11 entries:
   Num:    Value          Size Type    Bind   Vis      Ndx Name
     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND 
     1: 0000000000000000     0 FILE    LOCAL  DEFAULT  ABS inline_test_d.cc
     2: 0000000000000000     0 SECTION LOCAL  DEFAULT    1 
     3: 0000000000000000     0 SECTION LOCAL  DEFAULT    3 
     4: 0000000000000000     0 SECTION LOCAL  DEFAULT    4 
     5: 0000000000000000     0 SECTION LOCAL  DEFAULT    6 
     6: 0000000000000000     0 SECTION LOCAL  DEFAULT    7 
     7: 0000000000000000     0 SECTION LOCAL  DEFAULT    5 
     8: 0000000000000000    34 FUNC    GLOBAL DEFAULT    1 main
     9: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND _GLOBAL_OFFSET_TABLE_
    10: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND _ZN1A3fooEv
```

`inline_test_b.cc` knows this function but it's undefined. It will find `_ZN1A3fooEv` at link time if `foo()` is called on the spot where it is defined. Otherwise `foo()` won't have a symbol emitted.

[weak binding](https://unix.stackexchange.com/questions/478795/why-do-some-libc-symbols-have-weak-binding-and-others-global)


C++ encourages `inline` over macros.

It is advised to put all inline member functions outside the class body so that the interface and the implementation does not mix together. and the `inline` should not appear in the class declaration as it is an implementation detail.

Every function definition in a header file must be `inline`d so not to violate ODR. The `inline` keyword suppresses this, allowing multiple translation units to contain definitions.

## (C++17) inline variable

