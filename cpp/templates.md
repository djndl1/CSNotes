# Basics

## Function Templates

Function templates are functions that are parameterized so that they represent a family of functions.

Templates are compiled in two phases (names are checked twice, _two-phase lookup_):

1. Without instantiation at definition time, the template code itself is checked for correctness ignoring the template parameters.

2. At instantiation time, the template code is checked to ensure that all code is valid. All parts that depend on template parameters are double-checked.

When a template is used in a way that triggers its instantiation, a compiler will need to see its definition. The simplest approach is to implement each template inside a header file.

### Template Argument Deduction

Automatic type conversions are limited during type deduction:

1. When declaring call parameters by reference, conversions do not apply to type deduction. Two arguments declared with the same template parameter must match exactly.

2. When declaring call parameters by value, only trivial conversions that decay are supported, e.g. (`const`, `volatile` are ignored, references convert the to the referenced type, raw arrays or functions convert to the corresponding pointer type), the decayed types must match.

```cpp
template<typename T>
T max (T a, T b);

int const c = 42;
max(5, c); // T == int, const is ignored
int &ir = i;
max(i, ir); // T == int
int arr[4];
foo(&i, arr); // T == int*

// Automatic type conversion is not considered for deduced template paramters
max(4,2, 4); // error, no promotion. 

max(4.2, static_cast<double>(4)); // correct
max<double>(4.2, 4); // correct
```

Type deduction does not work for default call arguments. One solution is to declare a default argument.

```cpp
template<typename T = std::string>
void (T = "");

f(); // OK
```

When there is no connection between template and call parameters and when template parameters cannot be determined, the template argument must be explicitly specified. Template argument deduction does not take return types into account.

```cpp
template <typename T1, typename T2, typename RT>
RT max(T1 a, T2 b);

::max<int, double, double>(4, 7.2);
```

In general, all the argument types up to the last argument that cannot be determined implicitly must be specified.

```cpp
template <typename RT, typename T1, typename T2>
RT max(T1 a, T2 b);

::max<double>(4, 7.2);
```

(C++14) It is possible to let the compiler find out the return type by simply not declaring any return type.

```cpp
template <typename T1, typename T2>
auto max(T1 a, T2 b) // without trailing return type and always decay, no need for std::decay
{
    return b < a ? a : b;
}
```

Before C++14, this is done by

```cpp
template <typename T1, typename T2>
auto max(T1 a, T2 b) -> decltype(b < a ? a : b)
{
    return b < a ? a : b;
}
```

To prevent the return type from being deduced as a reference type:

```cpp
template <typename T1, typename T2>
auto max(T1 a, T2 b) -> typename std::decay(decltype(true ? a : b))::type
{
    return b < a ? a : b;
}
```

`std::common_type<>::type` yields the common type of two or more different types.

```cpp
template <typename T1, typename T2>
std::common_type<T1, T2>::type max(T1, T2);

// C++14
template <typename T1, typename T2>
std::common_type_t<T1, T2> max(T1, T2);
```
### Default Template Arguments

Template parameters can have default values:

```cpp
#include <type_traits>

template<typename T1, typename T2,
         typename RT = std::decay_t<decltype(true? T1() : T2())>>
RT max(T1 a, T2 b)
{
    return b < a ? a : b;
}

template<typename T1, typename T2,
         typename RT = std::common_type_t<T1, T2>>
RT max(T1 a, T2 b)
{
    return b < a ? a : b;
}

```

### Overloading

1. A nontemplate function can coexist with a function template that has the same name and can be instantiated with the same type. All other factors being equal, the overload resolution process preferes the nontemplate over one generated from the template. If the template can generate a function with a better match, the template is selected.

2. Multiple function templates with the same name can coexist:

Make sure all overloaded versions of a function are declared before the function is called, otherwise, some versions may not be seen when the overloading is resolved.

https://www.geeksforgeeks.org/passing-reference-to-a-pointer-in-c/

Passing by value in general is often better for:

1. the syntax is simple;

2. compilers optimize better;

3. move semantics often make copies cheap;

4. sometimes there is no copy or move at all;

5. a template might be used for both simple and complex types;

6. still, even designed for passing by value, it is possible to force passing by reference using `std::ref` and `std::cref`.

In general, function templates don't have to be declared with `inline` unless there are many specializations of templates for specific types.
