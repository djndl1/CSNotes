# `inline` in C99 is basically about linkage so that `inline` function is used only for inlining

Since C99, a function declaration with no declared linkage does not generate a function object. The inline definition will only be used with inline substitution, and the compiler is not obliged to perform this optimisation. It is expected that an external definition of the function exists in some other translation unit, and such a definition must exist if the function object is used, either by taking its address or by being called in a context where the compiler chooses not to perform the inline substitution. If the inline function is declared with either static or extern, then a function object is compiled, with the indicated linkage, thereby satisfying the requirement that the function object be defined.

The C99 standard says

>Any function with internal linkage can be an inline function. For a function with external linkage, the following restrictions apply: If a function is declared with an inline function specifier, then it shall also be defined in the same translation unit. If all of the file scope declarations for a function in a translation unit include the inline function specifier without extern, then the definition in that translation unit is an inline definition. An inline definition does not provide an external definition for the function, and does not forbid an external definition in another translation unit. An inline definition provides an alternative to an external definition, which a translator may use to implement any call to the function in the same translation unit. It is unspecified whether a call to the function uses the inline definition or the external definition.

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
}o
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

The `inline` version `foo()` which should return 3 is not used at all, not even emitted a symbol.

However, with optimization option the `inline` `foo()` is used but still without emitting a symbol. The address printed is the address of the `extern inline` `foo()`  which has external linkage.

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

Again, optimizing only the `inline` `foo()`, tells us that it really emitted a symbol.

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

Modify the `extern inline` `foo()` to `static inline` and recompile. See how two functions both got called and emitted symbols. `g()` uses the `foo()` defined in `inline_test_a.c` and `main` uses the `static inline foo()`. 

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

In C++, `inline` are treated by default as having external linkage, meaning that the program behaves as if there is only one copy of the function. Redefining an inline function with the same name but with different function body is illegal; however, the compiler does not flag this as an error, but simply generates a function body for the version defined in the first file entered on the compilation line and discards the others.

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

[inline functions](https://isocpp.org/wiki/faq/inline-functions)
[inline specifier](https://en.cppreference.com/w/cpp/language/inline)

C++ encourages `inline` over macros.

It is advised to put all inline member functions outside the class body so that the interface and the implementation does not mix together. and the `inline` should not appear in the class declaration as it is an implementation detail.

Every function definition in a header file must be `inline`d so not to violate ODR. The `inline` keyword suppresses this, allowing multiple translation units to contain definitions. However, an inline function, even if not declared as inline, must have its definiton in the same translation unit as the caller.

`extern inline` has the same semantics as `inline`

Removing one of the `inline` specifiers, the test program still compiles and uses the `inline`d one, which shows the external linkage of `inline`.

```shell
 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  g++ inline_test_b.cc inline_test_a.cc -o inline_test

 djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./inline_test 
foo called from main: return value = 3, address = 0x5594578ff18f
foo called from g: return value = 3, address = 0x5594578ff18f
```
