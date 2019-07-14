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
