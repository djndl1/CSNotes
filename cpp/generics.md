# Function and Variable Templates

C++ supports syntactic constructs allowing programmers to define and use completely general (or abstract) functions or classes, based on generic types and/or (possibly inferred) constant values. The template mechanism allows us to specify classes and algorithms, fairly independently of the actual types for which the templates are eventually going to be used. Templates play a central role in present-day C++, and should not be considered an esoteric feature of the language. 

For good reasons variadic functions are deprecated in C++. However, variadic templates tell us a completely different story, and variadic templates are perfectly acceptable. 

In general, function parameters should be specified as Type const & parameters to prevent unnecessary copying. The template mechanism is fairly flexible. Formal types are interpreted as plain types, const types, pointer types, etc., depending on the actually provided types.

Read more at [Template Argument Deduction](https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.2.0/com.ibm.zos.v2r2.cbclx01/template_argument_deduction.htm)

When the function template is called, the compiler must be able to infer not only Type’s concrete value, but also constant value.

`decltype`’s standard behavior: when provided with a variable’s name, it is replaced by that variable’s type. When an expression is used, the compiler determines whether a reference could be appended to the expression’s type. If so, decltype(expression) is replaced by the type of such an lvalue reference. If not, `decltype(expression)` is replaced by the expression’s plain type. `decltype(auto)` specifications can be used, in which case decltype’s rules are applied to auto. In practice, the `decltype(auto)` form is most often encountered with function templates to define return types. 

`declval`: TODO

Late-specified return type allows the use of `decltype` to define a function's return type. Any variable visible at the time decltype is compiled can be used in the decltype expression. 

```cpp
template <typename Lhs, typename Rhs>
auto add(Lhs, lhs, Rhs rhs) -> decltype(lhs + rhs)
{
    return lhs + rhs;
}
```

```cpp
std::string global{"hello world"};

template <typename MEMBER, typename RHS>
auto add(MEMBER mem, RHS rhs) -> decltype((global.*mem())() + rhs)
{
    return (global.*mem)() + rhs;
}
```

## Reference Wrappers `<functional>`

Situations exist where the compiler is unable to infer that a reference rather than a value is passed to a function template, or there should be cases where sometimes a value should be passed but a reference is sometimes required.

```cpp
template <typename Func, typename Arg>
void call(Func fun, Arg arg)
{
    fun(arg);
}

call(sqrtArg, value);
call(sqrtArg, ref(value));
```

## Local and unnamed types as template arguments

```cpp
enum {
    V1,
    V2,
    V3
};

template <typename T>
void fun(T &&t);

fun(V1);
```

```cpp
void definer()
{
    struct Local {
        double dVar;
        int    iVar;
    } local;
    
    fun(local);
}
```

## Template Parameter Deduction

When the compiler deduces the actual types for template type parameters it only considers the types of the arguments that are actually used.

```cpp
template <typename T>
T fun()
{
    return T{};
}
```

cannot be instantiated without specifying the type explicitly.

The compiler applies three types of parameter type transformations and a fourth one to function template non-type parameters when deducing the actual types of template type parameters:

- lvalue transformations: creating an rvalue from an lvalue.

```cpp
template <typename T>
T negate(T val)
{
    return -val;
}

template <typename T>
T sum(T *tp, size_t n)
{
    return accumulate(tp, tp + n, T());
}

template<typename T>
void call(T (*fp)(T), T const &val)
{
    (*fp)(val);
}

int main()
{
    int x = 5;
    x = negate(x); // lvalue x to rvalue (copies x)
    
    int y[10];
    sum(y, 10); // array-to-pointer transformation
    
    call(sqrt, 2.0); // function-to pointer transformation
}
```

- qualification transformations: inserting a `const` modifier to a non-constant argument type. This transformation is applied when the function template's type explicitly specifies `const` or `volatile` but the function argument isn't a `const` or `volatile` entity.

```cpp
template <typename T>
T negate(T const &val)
{
    return -val;
}
int main()
{
    int x = 5;
    x = negate(x); //
}
```

- transformation to a base class instantiated from a class template.

- standard transformations for function template non-type parameters.

The purpose of the various template parameter type deduction transformations is not to match function arguments to function parameters, but rather, having matched arguments to parameters, to determine the actual types of the various template type parameters.

The compiler uses the following algorithm to deduce the actual types:

1. the function template's parameters are identified in turn using the arguments of the called function;

2. The three transformations for template type parameters are applied where necessary.

With function templates the combination of the types of template arguments and template parameters shows some interesting contractions.  Doubling identical reference types are deduced to be a single reference type.

https://stackoverflow.com/questions/44115083/why-can-const-int-bind-to-an-int
https://stackoverflow.com/questions/36102728/why-is-it-allowed-to-pass-r-values-by-const-reference-but-not-by-normal-referenc

1. A function template parameter defined as an lvalue reference to a template’s type parameter (e.g., `Type &`) receiving an lvalue reference argument results in a single lvalue reference.

2. A function template parameter defined as an rvalue reference (forward reference) to a template’s type parameter (e.g., `Type &&`) receiving any kind of reference argument uses the reference type of the argument.

```
Actual & into Type & becomes Actual &
Actual & into Type && becomes Actual &
Actual && into Type & becomes Actual &
Actual && into Type && becomes Actual &&
```

## Declaring Function Templates

If multiple instantiations of a template using the same actual types for its template parameters exist in multiple object files the one definition rule is lifted. The linker weeds out superfluous instantiations.  In some contexts template definitions may not be required. Instead the software engineer may opt to declare a template rather than to include the template’s definition time and again in various source files. When templates are declared, the compiler does not have to process the template’s definitions again and again; and no instantiations are created on the basis of template declarations alone. Any actually required instantiation must then be available elsewhere.

To make sure that the required instantiation is available, we can explicitly instantiate a template.

```cpp
template int add<int>(int const &lhs, int const &rhs);
template double add<double>(double const &lhs, double const &rhs);
template string add<string>(string const &lhs, string const &rhs);
```

Funtion templates are instantiated when they are used or when addresses of function templates are taken.

```cpp
char (*addptr)(char const &, char const &) = add;
```

## Overloading Function Templates

When overloading function templates we do not have to restrict ourselves to the function’s parameter list. The template’s type parameter list itself may also be overloaded.

```cpp
template <typename Type>
Type add(Type const &lhs, Type const &rhs)
{
    return lhs + rhs;
}

template <typename Type>
Type add(Type const &lhs, Type const &mid, Type const &rhs)
{
    return lhs + mid + rhs;
}

template <typename Type>
Type add(std::vector<Type> const &vect)
{
    return accumulate(vect.begin(), vect.end(), Type());
}

template <typename Container, typename Type>
Type add(Container const &cont, Type const &init)
{
    return std::accumulate(cont.begin(), cont.end(), init);
}

template <typename Type, typename Container>
Type add(Container const &cont)
{
    return std::accumulate(cont.begin(), cont.end(), Type());
}

```

However, we have to explicitly specify `Type` since the compiler cannot determine that `Container` actually contains `int`.

```cpp
int x = add<int>(vectorOfInts);
```

As a rule of thumb: overloaded function templates must allow a unique combination of template type arguments to be specified to prevent ambiguities when selecting which overloaded function template to instantiate. The ordering of template type parameters in the function template’s type parameter list is not important.

## Specializing Templates for deviating types

A template explicit specialization defines the function template for which a generic definition already exists using specific actual template type parameters. A function template explicit specialization is not just another overloaded version of the function template. Whereas an overloaded version may define a completely different set of template parameters, a specialization must use the same set of template parameters as its non-specialized variant.

```cpp
template <typename Type>
Type add(Type const &lhs, Type const &rhs)
{
    return lhs + rhs;
}

char *add(char *const &lhs, char *const &rhs);
```

Template explicit specializations can be declared in the usual way. When declaring a template explicit specialization the pair of angle brackets following the template keyword are essential. If omitted, we would have constructed a template instantiation declaration.

```cpp
template <> char *add(char *const &p1, char *const &p2);
template <> char const *add(char const *const &p1, char const *const &p2);
```

If in addition template <> could be omitted the template character would be removed from the declaration. The resulting declaration is now a mere function declaration. This is not an error: function templates and ordinary (non-template) functions may mutually overload each other. Ordinary functions are not as restrictive as function templates with respect to allowed type conversions. This could be a reason to overload a template with an ordinary function every once in a while.

TODO

## Variables as Template

```cpp
template <typename T = long double>
constexpr T pi = T{3.1415926535897932385};

// specialization
template<>
constexpr char const *pi<char const *> = "pi";

```

# Class Template

Class template type parameter not being able to be deducted resulted in a proliferation of `make_*` functions.

```cpp
template <class ...T>
class Deduce {
public:
    Deduce(T ...params);
    void fun();
};

template <class T>
Deduce makePtr{static_cast<T*>(0)};
```

