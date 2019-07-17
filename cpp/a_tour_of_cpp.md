An overview of C++17

Advice about what constitutes good modern C++ can be found in the C++ Core Guidelines.

# Basics

- A declaration is a statement is a statement that introduces an entity into the program. It specifies a type for the entity.

- A type defines a set of possible values and a set of operations.

- An object is some memory that holds a value of some type.

- A value is a set of bits interpreted according to a type.

- A variable is a named object.

`'` can be used to make long literals more readable to humans.

## Initialization

```C++
double d1 = 2.3;
double d2 = {2.3};
double d3 {2.3}
```

If in doubt, use the general `{}` form. It prevents conversions that lose information. _Narrowing conversions_ are allowed and implicitly applied. Use `auto` when there is no need to state its type explicitly or to avoid redundancy and writing long type names, which is especially important in generic programming.

Two notions of immutability:

- `const`: "I promise to not to change this value". Used primarily to specify interfaces so that data can be passed to functions using pointers and references without ear of it being modified.

- `constexpr`: "to be evaluated at compile time". Primarily to specify constants, to allow placement of data in read-only memory and for performance. Must be calculated by the compiler.

__(C++-17)__ `if` statement can introduce a variable bound only inside the `if-else` blocks.

```c++
if (auto n = v.size(); n!=0) {
//...
}
```

A reference and a pointer both refer/point to an object and both are represented in memory as a machine address. However, the language rules for using them differ. Assignment to a reference does not change what the reference refers to but assigns to the referenced object,`*` is automatically (implicitly) done for a reference.

User-defined types are often preferred over built-in types because they are easier to use, less error-prone, and typically as efficient for what they do as direct use of built-in types, or even faster.

The use of "naked" unions is best minimized.
(C++-17)The standard library type, `variant` can be used to eliminate most direct uses of unions.

Enumerations are used to represent small sets of integer values. They are used to make code more readable and less error-prone than it would have been had the symbolic and enumerator names not bben used.


```cpp
enum class Color {red, blue, green};
Color y = Color::red;
```
At the language level, C++ represents interfaces by declarations.

# Modules (C++20)

`export`ing and `import`ing certain part of a source file as part of the interaface finally become possible.


```c++
//file Vector.cpp

module;
export module Vector;
export class Vector {
// definitions and declarations go here
};

//file user.cpp
import Vector;
#include <cmath>

//...
```

A module is compiled only once; two modules can be imported in either order without chaning their meaning; `import` is not transitive: a module is not imported automatically in a chain of `import`s.

# Error handling

The type system, the higher-level constructs simplify programming, limit opportunities for mistakes and increases the compiler's chances of catching errors.

A function can indicate that it cannot perform its alloted task by 

- throwing an exception

- somehow return a value indicating failure. 

- terminating the program (`terminate()`, `exit()`, `abort()`).

Exception handling is slow; it is often faster than correct handling of complex or rare error conditions and of repeated tests of error codes.

## Exceptions

Exceptions are designed to be used to report failure to complete a given task. Exceptions are integerated with constuctors and destructors to provide a coherent framework for error handling and resource management.

the implementation will unwind the function call stack as needed to get back to the context of that caller. That is, the exception handling mechanism will exit scopes and functions as needed to get back to a caller that has expressed interest in handling that kind of exception, invoking destructors along the way as needed. A function that should never throw an exception can be declared `noexcept`, where any `throw` inside the functions will turn into a `terminate()`.

In well-designed code, `try`-blocks are rare. Avoid overuse by systematically using the RAII technique.

Often, a function has no way of completing its assigned task after an exception is thrown. Then, “handling” an exception means doing some minimal local cleanup and rethrowing the exception.

## Resource Acquisition is Initialization

The basic idea behind RAII is for a constructor to acquire all resources necessary for a class to operate and have the destructor release all resources, thus making resource release guaranteed and implicit.

## Static Assertions

If an error can be found at compile time, it is usuall preferable to do so. The `static_assert` mechanism can be used to for anything that can be expressed in terms of expressions.

```c++
static_assert(4 <= sizeof(int), "integers are too small");

The most important uses of `static_assert` come whne making assertions about types used as parameters in generic programming.
```

# Function arguments and Return Values

Key concerns when passing information to and from functions: 

- copied or shared?

- If shared, is it mutable?

- Is an object moved, leaving an empty object behind?

## Argument Passing

When we care about performance, we usually pass small values by-value and larger ones by-reference. Here “small” means “something that’s really cheap to copy.” Exactly what “small” means depends on machine architecture, but “the size of two or three pointers or less” is a good rule of thumb. If we want to pass by reference for performance reasons but don’t need to modify the argument, we pass-by-const-reference.

Using a default argument is sometimes easier than overloading.

## Value return

We return “by reference” only when we want to grant a caller access to something that is not local to the function.

To pass large amounts of information out of a function, use move semantics, i.e. a move constructor or move assignment. Returning large objects by returning a pointer to it is common in older code and a major source of hard-to-find errors. 

## Structured binding

The mechanism for giving local names to members of a class object is called _structured binding_.

```c++
struct Entry {
    string name;
    int value;
};

Entry read_entry(istream& is)
{
    string s;
    int i;
    is >> s >> i;
    return {s, i};
}

auto e = read_entry(cin);
```

```c++
map<string, int> m;

for (const auto [key, value] : m) {
    cout << "{" << key << "," << value << "}\n";
}

void incr(map<string, int>& m)
{
    for (auto& [key, value] : m) {
        ++value;
    }
}
```

When structured binding is used for a class with no private data, it is easy to see how the binding is done: there must be the same number of names defined for the binding as there are nonstatic data members of the class, and each name introduced in the binding names the corresponding member.

It is also possible to handle classes where access is through member functions.

```c++
complex<double> z = {1, 2};
auto [re, im] = z + 2;
```
