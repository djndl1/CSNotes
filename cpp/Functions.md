# Function Objects

Function Objects are created by overloading the function call operator `operator()`. Function objects may truly be defined inline. Functions that are called indirectly (i.e., using pointers to functions) can never be defined inline as their addresses must be known. An added benefit of function objects is that they may access the private data of their objects. 

## Constructing a manipulator

 The class ostream has an overloaded `operator<<` accepting a pointer to a function expecting an `ostream &` and returning an `ostream &`; and another `operator<<` is defined
 
 ```cpp   
    inline _LIBCPP_HIDE_FROM_ABI_AFTER_V1
    basic_ostream& operator<<(basic_ostream& (*__pf)(basic_ostream&))
    { return __pf(*this); }

    inline _LIBCPP_HIDE_FROM_ABI_AFTER_V1
    basic_ostream& operator<<(basic_ios<char_type, traits_type>&
                              (*__pf)(basic_ios<char_type,traits_type>&))
    { __pf(*this); return *this; }

    inline _LIBCPP_HIDE_FROM_ABI_AFTER_V1
    basic_ostream& operator<<(ios_base& (*__pf)(ios_base&))
    { __pf(*this); return *this; }    
 ```
 
These operator overloadings does not accept manipulators with arguments like `setprecision(n)`.

```cpp
class __iom_t5
{
    int __n_;
public:
    _LIBCPP_INLINE_VISIBILITY
    explicit __iom_t5(int __n) : __n_(__n) {}

    template <class _CharT, class _Traits>
    friend
    _LIBCPP_INLINE_VISIBILITY
    basic_istream<_CharT, _Traits>&
    operator>>(basic_istream<_CharT, _Traits>& __is, const __iom_t5& __x)
    {
        __is.precision(__x.__n_);
        return __is;
    }

    template <class _CharT, class _Traits>
    friend
    _LIBCPP_INLINE_VISIBILITY
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const __iom_t5& __x)
    {
        __os.precision(__x.__n_);
        return __os;
    }
};

inline _LIBCPP_INLINE_VISIBILITY
__iom_t5
setprecision(int __n)
{
    return __iom_t5(__n);
}
```

Another way is to implement a manipulator as a class overloading `operator()` and `operator<<`.

```cpp
    class Align
    {
        unsigned d_width;
        std::ios::fmtflags d_alignment;

        public:
            Align(unsigned width, std::ios::fmtflags alignment);
            std::ostream &operator()(std::ostream &ostr) const;
    };

    Align::Align(unsigned width, std::ios::fmtflags alignment)
    :
        d_width(width),
        d_alignment(alignment)
    {}

    std::ostream &Align::operator()(std::ostream &ostr) const
    {
        ostr.setf(d_alignment, std::ios::adjustfield);
        return ostr << std::setw(d_width);
    }

    std::ostream &operator<<(std::ostream &ostr, Align &&align)
    {
        return align(ostr);
    }
```

# Lambda Expressions

A lambda expression defines an anonymous function object which may immediately be passed to functions expecting function object arguments.  Lambda expressions provide a concise way to create simple function objects. The emphasis here is on simple: a lambda expression’s size should be comparable to the size of inline-functions. If you need more code, then encapsulate that code in a separate function which is then called from inside the lambda expression’s compound statement, or consider designing a separate function object.

When a called function must remember its state a function object is appropriate, otherwise a plain function can be used.

A lambda expression defines an anonymous function object, also called a closure object or simply a
_closure_. When a lambda expression is evaluated it results in a temporary function object (the closure object). This temporary function object is of a unique anonymous class type, called its closure type.

```cpp
[]  // labmda-introducer
(int x, int y) // lambda-declarator
{
    return x + y; //normal compound statement
}
```

A lambda can be remove its `const`ness by specifying `mutable`, that is, `operator()(args...)` without `const` so that  by-value-captured variables can be modified.

```cpp
[] (int x, int y) mutable
...
```

The lambda-declarator may be omitted if no parameters are defined, but when specifying mutable (or constexpr, see below) a lambda-declarator must be specified (at least as an empty set of parentheses). The parameters in a lambda declarator cannot be given default arguments.

Declarator specifiers can be `mutable`, `constexpr`, or both. A `constexpr` lambda-expression is itself a `constexpr`, which may be compile-time evaluated if its arguments qualify as const-expressions. By implication, if a lambda-expression is defined inside a `constexpr` function then the lambda-expression itself is a `constexpr`, and the `constexpr` declarator specifier is not required.

If there are multiple return statements returning values of different types then the lambda expression’s return type must explicitly be specified using a late-specified return type

Although lambda expressions are anonymous function objects, they can be assigned to variables. Often, the variable is defined using the keyword `auto`. 

```cpp
auto sqr = [](int x) {
    return x * x
};
```

## Capturing variables

Variables visible at the location of a lambda expression may be accessible from inside the lambda expression’s compound statement.

When the lambda expression is defined inside a class member function the lambda-introducer may contain `this` or `∗this`.

Global variables are always accessible, and can be modified if their definitions allow so. Local variables of the lambda expression’s surrounding function may also be specified inside the lambda-introducer. Any capture may appear only once.



```cpp
[] (mutable)        // access to merely global variables
[this] (mutable)    // all the object's data members which can be modified
[*this]            // access to all the object's members which cannot be modified
[*this] mutable     // modifiable copies are used inside the lambda expression without affecting the object's own data

[local]
[this, local]
[*this, local]
// `local` is immutably accessed

[local] mutable
[this, local] mutable
[*this, local] mutable
// `local` is available as the a local copy

[&local] (mutable)
[this, &local] (mutable)
[*this, &local] (mutable)
// local is available by modifiable reference of the surrounding function's local variable
```

- `&` (implicitly capture the used automatic variables by reference) 
- `=` (implicitly capture the used automatic variables by copy).

```cpp
[=]
[=, this]           // (C++17)
[=, *this]          // (C++20)
// local variables are visible but not modifiable

[=] mutable
[=, this] mutable   // (C++17)
[=, *this] mutable // (C++20)
// local variables are visible as modifiable copies.

[=, &local] (mutable)
// `local` is accessed by modifiable reference

[&] (mutable)
[&, this] (mutable)
// local variables are visible as modifiable references

[&, *this] (mutable) // 

[&, local] (mutable)
[&, this, local] (mutable)
[&, *this, local] (mutable)
// `local` is accessed as a modifiable copy
```

The current object (`*this`) can be implicitly captured if either capture default is present. If implicitly captured, it is always captured by reference, even if the capture default is `=`.  Even when not specified, lambda expressions implicitly capture their `this` pointers, and class members are always accessed relative to `this`.

When a lambda captures a member using implicit by-copy capture, it does not make a copy of that member variable: the use of a member variable `m` is treated as an expression `(*this).m`, and `*this` is always implicitly captured by reference.

## Use

Named lambda expressions nicely fit in the niche of local functions: when a function needs to perform computations which are at a conceptually lower level than the function’s task itself, then it’s attractive to encapsulate these computations in a separate support function and call the support function where needed.

use lambda expressions sparingly. When they are used make sure that their sizes remain small. As a rule of thumb: lambda expressions should be treated like in-line functions, and should merely consist of one, or maybe occasionally two expressions.
