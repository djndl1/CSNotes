# Avoid name conflicts

It is possible to define a namespace using multiple namespace declarations. Both definitions and declarations are allowed inside a namespace.

Namespaces can be defined without a name, such an anynomous namespace restricts the visibility of the defined entities to the source file defining the anonymous namespace. In situations where in C static variables or functions would have been used, the anonymous namespace should be used in C++.

A `using` declaration can have restricted scope. It can be used inside a block.

# Koenig lookup

If a function is called without specifying its namespaces, then the namespace of its argument types are used to determine the function's namespace. If the namespaces in which the argument type are defined contains such a function, then the fucntion is used.

Definitions in one namespace may break the code defined in another namespace. Namespaces clearly are not completely independent of each other

```c++
namespace FBB
{
    struct Value
    {};
    void fun(int x);
    void gun(Value x);
}

namespace ES
{
    void fun(int x)
    {
        fun(x);
    }
    void gun(FBB::Value x)
    {
        gun(x);
    }
}
```

Here, `ES:gun()` doesn't even compile.

As a rule of thumb one might decide to stick to using declarations, up to the point where the list becomes impractically long, at which point a using directive could be considered.

Using declarations and directives should not be imposed upon code written by third parties. In practice this means that using directives and declarations should be banned from header files and should only be used in source files.

Namespaces can be nested. A long name like `ANS::BNS::c` is considered too long and can have an alias

```c++
namespace ABNS = ANS::BNS;
```

(C++17) when nesting namespacs, a nested namespace can be directly referred to using scope resolution operators.

```c++
namespace Outer::Middle::Inner {
    // all entities goes to Inner namespace
}
```

An entity can be defined outside of its namespace. Before an entity is defined outside of a namespace it must have been declared inside its namespace. To define an entity outside of its namespace its name must be fully qualified by prefixing the member by its namespaces. The definition may be provided at the global level or at intermediate levels in the case of nested namespaces. 
