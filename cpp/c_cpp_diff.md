# `main ` function

Only `int main()` or `int main(int argc, char **argv)` are allowed. The third `char **envp` parameter is not defined by the C++ standard and should be avoid. Instead, `extern char **environ` should be declared to provide access to the program's environment variables. The value of `argv[argc]` equals 0.
 
The return type is `int`, not `void`. `return` statement may be omitted, and `main` returns 0 in this case. When a C++ ends normally, dtors of globally defined objects are activated. A function like `exit` does not normally end a C++ program and is therefore deprecated.

# strict type checking

A prototype must be known for each funciton before it is called and the call must match the prototype. `return` must explicitly return `int` in `main`. An empty parameter list indicates the total absence of parameter, i.e. as with `void`.

# default arguments

Default arguments must be known at compile-time (instead of finding these arguments in the implementation at run-time). Therefore, the default arguments must be mentioned at the function's declaration.

# `NULL` pointer, `0` pointer and `nullptr`

In C++, all zero values are coded as 0.

`NULL` should be avoided. `0` pointer may lead to trouble with function overloading since it's actually `int`. Use `nullptr` instead.

# `#define __cplusplus`

Each C++ compiler which conforms to the ANSI/ISO standard defines the symbol `__cplusplus`.

# Using standard C functions

Normal C functions which are compiled and collected in a run-time library can be used in C++ programs.

`extern "C" void c_function(type args)` or

```c++
extern "C"
{
// C header if needed
// C-declarations here 
}
```

The comibination of the predefined symbol `__cplusplus` and the possibility to define `extern "C"` functions offers the ability to create header files for both C and C++.

```c++
#ifdef __cplusplus
extern "C"
{
#endif

/* declaration of C-data and functions are inserts here */

#ifdef __cplusplus
}
#endif
```

The standard C header files are built in this manner and are therefore usable for both C and C++.

# Defining local variables

Local variables should be defined only when they're needed. Variables should be defined in such a way that their scope is as limited and localized as possible. It is considered good practice to avoid global variables. In C++, global varaibles are seldom required.

If considered appropriate, _nested block_ can be used to localize auxiliary variables.

In C++, local variables can be defined and initialized within `if-else`, `switch`, `while` statements.

# `typedef`

The keyword `typedef` is not required anymore when defining `union`, `struct` or `enum` defintions.

```c++
struct someStruct {
//
}

someStruct whatVar;
```

# Evaluation order of operands

- Expressions using postfix operators (like index operators and member selectors) are evaluated from left to right.

- Assignment expressions are evaluated from right to left

- operands of shift operators are evaluated from left to right.

The overloaded operator is evaluated like the built-in operator it overloads.

# Attributes

Attributes are used to inform the compiler about situations that are intentional but are by themselves for the compiler to issue warnings.

- `[[fallthrough]]`: if falling through is intentional, this attribute should be used so that the compiler does not give a warning.

```c++
switch (selector) {
    case 1:
    case 2:
        ...
        [[fallthrough]]; // no warning
    case 3:
        ...
    case 4: // a warning
}
```

- `[[maybe_unused]]`: applied to a class, typedef-name, variable, parameter, non-static data member, a function, an enumeration or an enumerator. No warning is generated when the entity is not used.

- `[[nodiscard]]`: specified when declaring a function, class or enumeration. This attribute requires that the return value of a function may be ignored only when explicitly cast to void.

```c++
int [[nodiscard]] importantInt()

importantInt(); // warning issued
```
