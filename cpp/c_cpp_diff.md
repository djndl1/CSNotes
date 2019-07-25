# `main ` function

Only `int main()` or `int main(int argc, char **argv)` are allowed. The third `char **envp` parameter is not defined by the C++ standard and should be avoid. Instead, `extern char **environ` should be declared to provide access to the program's environment variables. The value of `argv[argc]` equals 0.
 
The return type is `int`, not `void`. `return` statement may be omitted, and `main` returns 0 in this case. When a C++ ends normally, dtors of globally defined objects are activated. A function like `exit` does not normally end a C++ program and is therefore deprecated.

# strict type checking

A prototype must be known for each funciton before it is called and the call must match the prototype. `return` must explicitly return `int` in `main`. An empty parameter list indicates the total absence of parameter, i.e. as with `void`.

# Strongly types enumerations

The old style `enum` has their values not restricted to the enum type name itself, but to the scope where the enumeration is defined. Two enumerations having the same scope cannot have identical names.

`enum class` solves such problems. The value type used by `enum class` can be specified instead of `int` only.

```c++
enum class CharEnum : unsigned char {
    NOT_OK,
    OK
}
```

`...` operator is allowed to show a sequence of symbols of a `enum class`.

# `NULL` pointer, `0` pointer and `nullptr`

In C++, all zero values are coded as 0.

`NULL` should be avoided. `0` pointer may lead to trouble with function overloading since it's actually `int`. Use `nullptr` instead.


# default arguments

default arguments must be known at compile-time (instead of finding these arguments in the implementation at run-time). Therefore, the default arguments must be mentioned at the function's declaration.


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

# `typedef` and `using`

The keyword `typedef` is not required anymore when defining `union`, `struct` or `enum` defintions.

```c++
struct someStruct {
//
}

someStruct whatVar;
```
The scope of typedefs is restricted to compilation units. Therefore, typedefs are usually embedded in
header files which are then included by multiple source files in which the typedefs should be used.

In practice, `typedef` and `using` can be used interchangeably.

```c++
typedef unsigned long long int FUN(double, int);
using FUN = unsigned long long int (double, int);
using FUN = auto (double, int) -> unsigned long long int;
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

- `[[noreturn]]`: used in functions like `std::terminate`, `std::abort`.

- `[[deprecated]]`/`[[deprecated("reason")]]`: 

```c++
int [[nodiscard]] importantInt()

importantInt(); // warning issued
```

# `const` keyword

In C++, variables declared `const` can be used to specify the size of an array.

# global namespace and scope resolution operator `::`

```c++
#include <cstdio>

double counter = 50;

int main()
{
    for (int counter = 1; counter != 10; counter++) {
        printf("%d\n", ::counter / counter); //global `counter` divided by local `counter`
    }
}
```

# Stream objects `cin`, `cout`, `cerr`

Some advantages of using streams are:

- Using insertion and extraction operators is _type-safe_. Old style functions may be given wrong format specifier. With streams there are no format strings.

- Insertion and extraction may be extended, allowing objects of classes to be inserted into or extracted form streams.

- Streams are independent of the media they operate on.

# References

- When a function explicitly must change the values of its arguments, a pointer parameter is preferred. These pointer parameters should preferably be the function’s initial parameters. This is called return by argument. If the modification of the argument is a trivial side-effect, references can be used.



# Initializer lists

C++ extends the concept of initializer list by introducing the type `initializer_list<Type>` where `Type` is reolaced by the type name of the values used in the initializer list.

Initializer lists are recursive, so they can be used with multidimensional arrays, structs and clases.

```c++
void values2(std::initializer_list<std::initializer_list<int>> iniValues)
{}
values2({{1, 2}, {2, 3}, {3, 5}, {4, 7}, {5, 11}, {6, 13}});
```

# Designated initialization

As C++ requires that destruction of data members occurs in the opposite order as their construction it is required that, when using designated initialization, members are initialized in the order in which they are declared in their class or struct. A union can be initialized using designated initialization.

In C++, it is not allowed to reorder the initialization of members in a designated initialization list.

# Initializer for bit fields

(C++2a) Bit fields is allowed them to be initialized by default by using initialization expressions in their definitions.

```c++
struct FirstIP4word
{
    uint32_t version: 4 = 1; // version now 1, by default
    uint32_t header: 4 = 10; // TCP header length now 10, by default
    uint32_t tos: 8;
    uint32_t length: 16; 
};
```

# Type inference using `auto`

The keyword `auto` can be used to simplify type definitions of variables and return types of functions if
the compiler is able to determine the proper types of such variables or functions. It is no longer used as a storage class specifier.

Plain types and pointer types are used as-is when declared `auto`. A reference's basic type (without the reference, omitting `const` and `volatile`)  is used. If a reference is required, use `auto&` or `auto&&`. Likewise, `const` and/or pointer specifications can be used in combination with the `auto` keyword.

The declaration of such a function `int (*intArrPtr())[10];` is rather complex. Using `auto`, it becomes

```c++
auto intArrPtr() -> int (*)[10];
```

which is called a _late-specified return type_.

(C++14) Late return type specifications are no longer required for functions returning auto, simply

```c++
auto autoReturnFunction();
```

in which case, all return values must have an identical type. Funtions merely returning `auto` cannot be used before the compiler has seen their definitions. So they cannot be used after mere declarations. When such functions are implemented as recursive function, at least one return statement must have been seen before the recursive call.

```c++
auto fibonacci(size_t n) 
{
    if (n <=1 )
        return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

# Structured binding declarations (C++17)

Usually, when functions need to return mutliple values, a return-by-argument construction is often used. When multiple vlaues should be returned from a function, a struct can be used.

```c++
struct Return {
    int first;
    double second;
};

Return fun() 
{
    return { 1, 12.5};
}

Return& fun2()
{
    static Return ret{4, 5};
    return ret;
}
```

The struct definition can completely be omitted if fun returns a pair or tuple. Instead of referring to the elements of the returned struct, pair or tuple structured binding declarations can also be used.

```c++
auto [one, two] = fun();
auto&& [rone, rtwo] = fun();
auto& [lone, ltwo] = fun2();
```

There doesn't have to be a function call!

```c++
auto const &[lone, ltwo] = Return{4, 5};
auto &&[lone, ltwo] = Return{4, 5};

for (auto &[year, amount, interest] : structArray)
    cout << "Year " << year << ": amount = " << amount << '\n';
```

The object doesn't even have to make its data member publicly available (TODO).

# Range-based for-loops

- Plain arrays

- Initializer lists;

- standard containers

- any other type offering `begin()` and `end()` functions returning iterators.

(C++20) range-based for-loop can have a init-statement.

# (C++17) `if`, `switch` with init-statement

Before using the condition clauses an initialization clause may be used to define additional variables (plural, as it may contain a comma-separated list of variables, similar to the syntax that’s available for for-statements).


# Raw String literals

Raw string literals start with an `R`, followed by a double quote, optionally followed by a label (which is an arbitrary sequence of characters not equal to `(`, followed by `(`. The raw string ends at the closing parenthesis ), followed by the label (if specified when starting the raw string literal), which is in turn followed by a double quote.

```c++
R"label(whatever raw string you want)label"
```

```c++
char const *noPrompt =
R"(
    if (d_debug__)
        s_out__ << '\n';
)";
```

# Binary constants (C++14)

Binary integral constants can be defined using the prefixes `0b` or `oB`.

# New language-defined data types

There is a subtle issue to be aware of when converting applications developed for 32-bit architectures to 64-bit architectures. When converting, only `long` types and pointer types change in size from 32 bits to 64 bits. `int` remains at 32 bits.

`L` as a prfix is used to indicate a character string whose elements are `wchar_t`. `p` specifies the power in hexadecimal floating point numbers, the exponential part is interpreted as a power of 2.

```c++
0x10p2 // 16 * 2^2 = 64
```

If a function should inform its caller about the success or failure of its task, let the function return a bool value. If the function should return success or various types of errors, let the function return enum values, documenting the situation by its various symbolic constants.

## Unicode encoding

C++ supports 8, 16 and 32 bit Unicode encoded strings. Two new data types are introduced: `char16_t`, `char32_t` representing UTF-16 and UTF-32 respectively. A `char` type value fits in a UTF-8 unicode value.

```c++
char utf_8[] = u8"This is UTF-8 encoded.";
char16_t utf16[] = u"This is UTF-16 encoded.";
char32_t utf32[] = U"This is UTF-32 encoded.";

char utf_8[] = u8"\u2018";
char16_t utf16[] = u"\u2018";
char32_t utf32[] = U"\u2018";

```


# Casts

C++ prorams should merely use the new style C++ casts as they offer the compiler facilities to verify the sensibility of the cast.

## `static_cast`

The `static_cast<type>(expression)` is used to convert ‘conceptually comparable or related types’ to each other.

```c++
sqrt(static_cast<double>(x) / y);
cout << static_cast<int>(Enum::VALUE);
tolower(static_cast<unsigned char>(ch));
```

The `static_cast` is used in the context of class inheritance to convert a pointer to a derived class to a pointer to its base class. Also, use `static_cast` to convert `void *` to an intended destination pointer.

## `const_cast`

A const_cast<type>(expression) expression is used to undo the const attribute of a (pointer) type.

The need for a const_cast may occur in combination with functions from the standard C library which traditionally weren’t always as const-aware as they should.

## `dynamic_cast`

Different from the static_cast, whose actions are completely determined compile-time, the `dynamic_cast`’s actions are determined run-time to convert a pointer to an object of some class.

## `reinterpret_cast`

`reinterpret_cast` should only be used when it is known that the information as defined in fact is or can be interpreted as something completely different. Think of the reinterpret_cast as a cast offering a poor-man’s union: the same memory location may be interpreted in completely different ways. Avoid this unless necessary.


```c++
reinterpret_cast<pointer type>(pointer expression)
```

```c++
cout.write(reinterpret_cast<char const *>(&value), sizeof(double)); // value is a double variable
```
