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

With nested classes,

```
template <typename OuterType>
class Outer {
public:
    template <class InnerType>
    struct Inner {
        Inner(OuterType);
        Inner(OuterType, InnerType);
        template <typename ExtraType>
        Inner(ExtraType, InnerType);
    };
};

Outer<int>::Inner inner{2.0, 1};
```

To deduct template arguments: ???

- first, a list of constructors is formed. 

- For each element of of the list, a parallel imaginary function is formed by the compiler, with the return type the class types of the constructors, using the template parameter of the original class template.

- Ordinary argument deduction and overload resolution is applied to the set of imaginary functions.

The signature of a constructor may be independent of the template type parameter. In such cases, the ???

https://en.cppreference.com/w/cpp/language/class_template_argument_deduction


## Constructor a Class Template


Members (functions or nested classes) of class templates that are themselves templates are called
member templates. When a template member is implemented below its class interface, the template class header must precede the function template header of the member template;

```cpp
#include <algorithm>
#include <iterator>
#include <cstddef>
#include <utility>
#include <stdexcept>
#include <cstring>

template <typename Data>
class CirQue {
private:
    size_t  d_size;
    size_t  d_maxSize;
    Data   *d_data;
    Data   *d_front;
    Data   *d_back;

    Data *inc(Data *ptr)
        {
            ++ptr;
            return ptr == d_data + d_maxSize ? d_data : ptr;
        }

public:
    typedef Data value_type;
    typedef value_type const &const_reference;

    template <size_t Size>
    explicit
    CirQue(Data const (&arr)[Size]) : d_maxSize{Size}, d_size{0},
                                      d_data{static_cast<Data*>(operator new(Size * sizeof(Data)))},
                                      d_front{d_data}, d_back{d_data}
        {
            std::copy(arr, arr + Size, std::back_inserter(*this));
        }

    CirQue(Data const *data, size_t size) : d_maxSize{size}, d_size{0},
                                            d_data{static_cast<Data*>(operator new(size * sizeof(Data)))},
                                            d_front{d_data}, d_back{d_data}
        {
            std::copy(data, data + size, std::back_inserter(*this));
        }

    explicit
    CirQue(size_t d_maxSize): d_size{0}, d_maxSize{d_maxSize},
                            d_data(d_maxSize == 0 ? 0 :
                                   static_cast<Data *>(operator new(d_maxSize * sizeof(Data)))),
                            d_front{d_data}, d_back{d_data}
        {}

    CirQue(CirQue<Data> const &other) : d_size{other.d_size}, d_maxSize{other.maxSize()},
                                        d_data{d_maxSize == 0 ? 0 :
                                               static_cast<Data*>(operator new(d_maxSize * sizeof(Data)))},
                                        d_front{d_data + (other.d_front - other.d_data)}
        {
            Data const *src = other.d_front;
            d_back = d_front;
            for (size_t count = 0; count != d_size; ++count) {
                new(d_back) Data(*src); // placement new
                d_back = inc(d_back);
                if (++src == other.d_data + d_maxSize)
                    src = other.d_data;
            }
        }

    CirQue(CirQue<Data> &&tmp) : d_data{nullptr}
        {
            swap(tmp);
        }

    ~CirQue()
        {
            if (d_data == 0)
                return;
            for (; d_size--; ) {
                d_front->~Data(); // memory allocation is done in one shot
                d_front = inc(d_front);
            }
            operator delete(d_data);
        }

    Data &back()
        {
            return d_back == d_data ? d_data[d_maxSize - 1] : d_back[-1];
        }

    Data &front()
        {
            return *d_front;
        }

    bool empty() const
        {
            return d_size == 0;
        }

    bool full() const
        {
            return d_size == d_maxSize;
        }

    size_t size() const
        {
            return d_size;
        }

    size_t maxSize() const
        {
            return d_maxSize;
        }

    CirQue &operator=(CirQue<Data> const &rhs)
        {
            CirQue<Data> tmp{rhs};
            swap(tmp);
            return *this;
        }

    CirQue &operator=(CirQue<Data> &&tmp)
        {
            swap(tmp);
            return *this;
        }

    void pop_front()
        {
            if (d_size == 0)
                throw std::out_of_range("Empty Queue!");
            d_front->~Data();
            d_front = inc(d_front);
            --d_size;
        }

    void push_back(Data const &object)
        {
            if (d_size == d_maxSize)
                throw std::out_of_range("Full queue!");
            new(d_back) Data(object);
            d_back = inc(d_back);
            ++d_size;
        }

    void swap(CirQue<Data> &other)
        {
            static size_t const size = sizeof(CirQue<Data>);

            char tmp[size];
            std::memcpy(tmp, &other, size);
            std::memcpy(reinterpret_cast<char*>(&other), this, size);
            std::memcpy(reinterpret_cast<char*>(this), tmp, size);
        }
};
```
When objects of a class template are instantiated, only the definitions of all the template’s member
functions that are actually used must have been seen by the compiler.

Even though default arguments can be specified, the compiler must still be informed that object definitions refer to templates. When instantiating class template objects using default template arguments the type specifications may be omitted but the angle brackets must be retained. When a class template uses multiple template parameters, all may be given default values. Like default function arguments, once a default value is used all remaining template parameters must also use their default values.

Class templates may also be declared. Default template arguments cannot be specified for both the declaration and the definition of a class template. As a rule of thumb default template arguments should be omitted from declarations, as class template declarations are never used when instantiating objects but are only occasionally used as forward references.

In C++ templates are instantiated when the address of a function template or class template object is taken or when a function template or class template is used. It is possible to (forward) declare a class template to allow the definition of a pointer or reference to that template class or to allow it being used as a return type.  C++ allows programmers to prevent templates from being instantiated, using the `extern` template syntax.   The compiler assumes (as it always does) that what is declared has been implemented elsewhere.  The instantiations of the templates must be available before the linker can build the final program.

```cpp
extern template class std::vector<int>;
```

## Generic Lambda Expressions

Generic lambda expressions may use `auto` to define their parameters. Generic lambda expressions are in fact class templates.

```cpp
auto lambda = [](auto lhs, auto rhs)
{
     return lhs + rhs;
};
```

For generic lambdas, capturing outer scope variables has no restrictions on whether variables are acptured by value or by reference.

```cpp
std::unique_ptr<int> ptr(new int(10));
auto fun = [value = std::move(ptr)] {
    return *value;
}
```

```cpp
//compile with concepts option
auto accumulate(auto const &container, auto function)
{
    auto accu = decltype(container[0]){};

    for (auto &value: container)
        accu = function(accu, value);

    return accu;
}

auto lambda = [](auto lhs, auto rhs)
            {
                return lhs + rhs;
            };

int main()
{
    vector<int> values  = {1, 2, 3, 4, 5};
    vector<string> text = {"a", "b", "c", "d", "e"};

    cout << accumulate(values,  lambda) << '\n' <<
            accumulate(text,    lambda) << '\n';
}
```

Generic lambda functions can also be defined like ordinary tempates, in which case the template header immediately follows the lambda-introducer.

```cpp
auto generic = []<typename Type>(Type &it, typename Type::ValueType value) {
    typename Type::ValueType val2{value};
    Type::staticMember();
}
```

## Static data members

When static members are defined in class templates, they are defined for every new type for which the class template is instantiated.  They are only declared and must be defined separately. With static members of class templates this is no different. The definitions of static members are usually provided immediately following the template class interface.

## `typename`

`typename` is also used to disambiguate code inside templates.

```cpp
template <typename Type>
Type function(Type t)
{
    typename Type::Ambiguous *ptr; // otherwise, it would be the multiplication of two variables
    
    return *ptr + t;
}
```

When such subtypes appear inside template definitions as subtypes of template type parameters the typename keyword must be used to identify them as subtypes.

```cpp
template <typename Container>
class Handler
{
    Container::const_iterator d_it; //error, Container::const_iterator is taken as a static member
public:
    Handler(Container const &container)
    :
    d_it(container.begin())
    {}
};
```

Typenames can be embedded in typedefs. As is often the case, this reduces the complexities of dec-
larations and definitions appearing elsewhere.

## Specialization class templates for deviating types

When considering a specialization one should also consider inheritance. the inherited class inherits the members of its base class while the specialization inherits nothing. 

A template specialization is recognized by the template argument list following a function or class template’s name and not by an empty template parameter list. Class template specializations may have non-empty template parameter lists. If so, a partial class template specialization is defined.

TODO

## Variadic Templates

Variadic templates allow us to specify an arbitrary number of template arguments of any type. Variadic templates were added to the language to prevent us from having to define many overloaded templates and to be able to create type safe variadic functions.


```cpp
template <typename ...params> class Variadic;
```

Parameter pack can be used to bind type and non-type template arguments to template parameters. The ellipsis to the right of the template pack's parameter name is the _unpack operator_ as it unpacks a series of arguments in a function's argument list.

```cpp
template <typename ...Params>
struct StructName {
    enum: size_t { s_size = sizeof ...(Params) };
};

StructName<int, char>::s_size; // 2
```

The argument associated with a variadic template parameter are not directly available to the implementation of a function or class template. By defining a partial specialization of a variadic template, explicitly defining an additional template type parameter, we can associate the first template argument of a parameter pack with this additional (first) type parameter.

```cpp
template <typename First, typename ...Params>
void printcpp(std::string const &format, First value, Params ...params)
{
    size_t  left = 0;
    size_t  right = 0;
    while (true) {
        if ((right = format.find('%', right)) == string::npos)
            throw std::runtime_error("printcpp: too many arguments");
        if (format.find("%%", right) != right)
            break;
        
        // output '%'
        ++right;
        std::cout << format.substr(left, right - left);
        left = ++right;
    }
    std::cout << format.substr(left, right - left) << value;
    printcpp(format.substr(right + 1), params...);
}

void printcpp(string const& format)
{
    size_t left = 0;
    size_t right = 0;
    
    while (true) {
        if ((right = format.find('%', right)) == string::npos)
            break;
        if (format.find("%%", right) != right)
            throw std::runtime_error("printcpp: missing arguments);
        ++right;
        std::cout << format.substr(left, right-left);
        left = ++right;
    }
    std:;cout << format.substr(left);
}
```

## Perfect Forwarding

With perfect forwarding the arguments passed to functions are ‘perfectly forwarded’ to nested functions. Forwarding is called perfect as the arguments are forwarded in a type-safe way.

The forwarding function is defined as a template (usually a variadic template, but single argument forwarding is also possible). `std::forward` is used to forward the forwarding function’s arguments to the nested function, keeping track of their types and number.

```cpp
class Inserter {
    std::string d_str;
    
public:
    Inserter();
    Inserter(std::string const &str);
    Inserter(Inserter const &other);
    Inserter(Inserter &&other);
    
    template <typename ...Params>
    void insert(Params &&... params)
    {
        d_str.insert(std:;forward<Params>(params)...);
    }
};
```

About `std::forward`

```cpp
#include <type_traits>
#include <utility>

#include <iostream>

void lref_fun(int& a)
{
    std::cout << "lref" << std::endl;
}

void rref_fun(int&& a)
{
    std::cout << "rref" << std::endl;
}

int main(int argc, char *argv[])
{
    int b = 5;

    rref_fun(std::forward<int>(b));
    lref_fun(std::forward<int&>(b)); // when perfectly forwarding, this is what happens to lvalue
    return 0;
}
```

No mechanism other than recursion is available to obtain the individual types and values of a variadic template.

https://stackoverflow.com/questions/8526598/how-does-stdforward-work

TODO



Folding expression TODO

## Tuples 

TODO


## Template Argument Deduction

Top level `const`s in either the parameter or the argument are ignored. A function parameter that is a reference or pointer to a `const` can be passed a reference or pointer to a nonconst object.

CPPSTD17-17.8.2
