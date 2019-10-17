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
max(i, ir); // T == intint arr[4];
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

## Class Templates

Inside a class template, using the class name not followed by template arguments represents the class with its template parameters its arguments.

For class templates, member functions are instantiated only if they are used.

Template arguments only have to provide all necessary operations that are needed instead of all operations that the template requires.

## Concept

Since C++11, some basic constraints are checked by using `static_assert` and some predefined type traits.

TODO

## Friends

A friend function can be defined inside a class template, without redeclaring the type parameter. When trying to declare the friend function and define it afterwards:

- implicitly declare a new function template, which must use a different template parameter:

```cpp
template <typename T>
class Stack {
    ...
    template <typename U>
    friend std::ostream& operator<< (std::ostream&, Stack<U> const &);
};
```

- forward declare the output operator to be a template

```cpp
template <typename T>
class Stack;

template <typename T>
std::ostream& operator<<(std::ostream&, Stack<T> const&);

template <typename T>
class Stack {
    friend std::ostream& operator<< <T>(std::ostream&, Stack<T> const&); // a specialization of a nonmember function template as friend
};
```

## Specialization of Class Templates

Specializing class templates allows for optimization of implementations for certain types or to fix a misbehavior of certain types for an instantiation of the class template. All member functions must be specialized if a class template is specialized.

```cpp
template<>
class Stack<std::string> {
private:
    std::deque<std::string> elems;

public:
    void push(std::string const&);
    void pop();
    std::string const &top() const;
    bool empty() const { return elems.empty(); }
};
```

### Partial Specialization

```cpp
template<typename T>
class Stack<T*> {
private:
    std::vector<T*> elems;

public:
    void push(T*);
    T* pop();               // a slightly different interface
    T* top() const;
    bool empty() const { return elems.empty(); }
};
```

Class templates might also specialize the relationship between multiple template parameters:

```cpp
template <typename T1, typename T2>
class MyClass {
    // ...
};

template <typename T>
class <T,T> {
    // ...
};

template <typename T>
class MyClass<T, int> {
    // ...
};

template <typename T>
class MyClass<T*, T*> {
    // ...
    
};

MyClass<int, int> m; // error, ambiguous partial specilization;
```

If more than one partial specialization matches equally well, the declaration is ambiguous:

An alias declaration using `using` can be templated to provide a convenient name for a family of types, called _alias template_.

```cpp
template <typename T>
using DequeStack = Stack<T, std::deque<T>>;

DequeStack<int> // == Stack<int, std::deque<int>>

template <typename T>
using MytypeIterator = typename Mytype::iterator;
```

(C++14) The standard library uses this technique to define shortcuts for all type traits that yields a type.

```cpp
template <class _Tp> using decay_t = typename decay<_Tp>::type;
template <class ..._Tp> using common_type_t = typename common_type<_Tp...>::type;
```

### (C++17) Class Template Argument Deduction

The constructor is reponsible for deducing template parameters (that don't have a default value). 

Be careful when using a string literal, this may result in weird paramter type deduction:

```cpp
// with Stack(T const& elm);
Stack stringStack = "bottom"; // Stack<char[7], std::vector<std::allocator<char[7]>>>

// with Stack(T const elm);
Stack stringStack = "bottom"; // Stack<const char*, std::vector<std::allocator<const char*>>>
```

In general, when passing arguments of a template type T by reference, the parameter doesn't decay. When passing by value, the parameter decays.

Instead of declaring the constructor to be called by value, we should disable automatically deducing raw character pointers for container classes by using deduction guides:

```cpp
Stack(const char*) -> Stack<std::string>;
```

This guide has to appear in the same scope as the class definition.

[C++ Initialization Hell](mikelui.io/2019/01/03/seriously-bonkers.html)

```cpp
Stack<std::string> strst = "abc"; // error

//no known conversion from 'const char [4]' to 'const std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >' for 1st argument
//    Stack(T const elm);
```


### (C++17) Deduction Guide for Aggregate Class Templates

```cpp
template <typename T>
struct ValueWithComment {
    T value;
    std::string comment;
};

ValueWithComment(char const*, char const*) -> ValueWithComment<std::string>;

ValueWithComment vc2 = {"hello" ,"initial value"};
```

# Nontype Template Parameters

```cpp
template <typename T, std::size_t Maxsize>
class Stack {
private:
    std::array<T, Maxsize> elems;
    std::size_t numElems;
public:
    Stack();
    void push(T const& elem);
    void pop();
    T const& top() const;
    bool empty() const { return elems.empty(); }
    std::size_t size() const { return numElems; }
};
```

In general, non-type parameters can be only constant integral values (including enumerations), pointers to objects/functions/members, lvalue references to objects or functions, or `std::nullptr_t`. Floating-point numbers and class-type objects are not allowed as nontype template parameters. When passing template arguments to pointers or references, the objects must not be string literals, temporaries, or data members and other subobjects. __(C++ mess warning!)__ Different standards have different additional linkage constraints. In C++17, it can have [no linkage](https://stackoverflow.com/questions/24864840/difference-between-internal-and-no-linkage).

[Linkage](https://en.wikipedia.org/wiki/Linkage_(software))
[Internal and External Linkage in C++](http://www.goldsborough.me/c/c++/linker/2016/03/30/19-34-25-internal_and_external_linkage_in_c++/)

## (C++17) Template Paramter Type `auto`

A nontype template parameter can accept genrically any type that is allowed for a nontype parameter.

```cpp
template <typename T, auto Maxsize>
class Stack {
private:
    std::array<T, Maxsize> elems;
    std::size_t numElems;
public:
    using size_type = decltype(Maxsize);

    Stack();
    void push(T const& elem);
    void pop();
    T const& top() const;
    bool empty() const { return elems.empty(); }
    auto size() const { return numElems; } // C++14 auto as a return type
};
```

```cpp
Stack<int, 20u> int20Stack; // uint 20
Stack<std::string, 40> stringStack; // int 40

std::is_same<decltype(size1), decltype(size2)>::value; // they dont have the same type
// Also
_LIBCPP_INLINE_VAR _LIBCPP_CONSTEXPR bool is_same_v
    = is_same<_Tp, _Up>::value;
```

Also, it's possible to pass strings as constant arrays:

```cpp
template <auto T>
class Message {
// ...
};

static char const s[] = "hello";
Messsage<s> msg2;
```
Even `template<decltype<auto> N>` is possible, allowing isntantiation of `N` as a reference.

```cpp
template <decltype(auto) N>
class C {};

int i;
C<(i)> n; // N is int&, note the brackets.
```

[Note how decltype declares a reference type](https://en.cppreference.com/w/cpp/language/decltype)
