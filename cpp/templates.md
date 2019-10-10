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

max(4,2, 4); // error, no promotion
max(4.2, static_cast<double>(4)); // correct
max<double>(4.2, 4); // correct
```

Type deduction does not work for default call arguments. One solution is to declare a default argument.

```cpp
template<typename T = std::string>
void (T = "");

f(); // OK
```

