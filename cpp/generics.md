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
