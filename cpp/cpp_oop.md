# Classes

In C++ `struct` finds little use, they are mainly used to aggregate data within the context of classes or to define elaborate return values. C++ extends the C `struct` and `union` concepts by allowing the definition of member functions within these data types.

Use `const` reference to return a private member to prevent a backdoor, that is, modifying objects without going through a mutator.

The global object is constructed first, then first objects in the `main` function, finally those in other functions.

The compiler choose to let the declaration interpretation prevail over the definition. Prefer to use `{}` to call constructor if possible.

```c++
// Data is some class

Data d1(); // actually declared a function
Data d2(int()) // // int() here is a function pointer type `int (*)()`
```

or an assignment operator

```cpp
Data d1 = Data();
```

And the compiler will try to remove superfluous parentheses.

```cpp
Data(b); // define b as a Data object
```

## Object Construction and Initialization

Only the constructors of objects of classes comprising a class  will be called automatically if not explicitly called, variables of primitive types are not initialized. The order in which class type data members are initialized is defined by the order in which those members are defined in the composing class interface. Member initializers should be used as often as possible. To initialize a reference member variable, use initializer (Avoid using reference member). Use a reference member when you want the life of your object to be dependent on the life of other objects.

C++ supports data member initialization, which allows us to assign initial values to data members. The initial values do not have to be constant expressions. All constructors will apply the data member initializations unless explicitly initialized otherwise.

```c++
class Container {
    Data *d_data = 0;
    size_t d_size = 0;
    size_t d_nr = ++s_nObjects;
    
    static size_t s_nObjects;
    
    public:
        Container() = default;
        Container(Container const &other);
        Container(Data* data, size_t size);
        Container(Container &&tmp);
}
```

```cpp
class Stat {
    bool d_hasPath = false;
    
    public:
        Stat(std::string const &fileName, std::sting const &searchPath) : d_hasPath(true) 
        {
        //
        }
}
```

An _aggregate_ is an array or a class/struct (with no used-defined constructors, no private or protected non-static data members, no base classes and no virtual functions)

```cpp
struct POD {
    int first = 5;
    double second = 1.28;
    std::string hello{"hello};
}

POD pod{4, 13.5, "hi there};
POD pod{4}; //  `first` initialized to 4
```

Often constructors are specializations of each other, allowing objects to be constructed specifying only subsets of arguments for all of its data members, using default argument values for the remaining data members. C++ offers constrctor delegation.

If a constructor supporting an initializer list is available, the compiler chooses the initializer list over the uniform initialization. To use the one-argument constructor, the standard constructor syntax must be used.

A trivial default constructor performs the following actions:

- Its data members of built-in or primitive are not initialized;

- Its composed(class type) data members are initialized by their default constructors.

- If the class is a derived class, the base class is initialized by its default constructor.

C++ offers the `= default` syntax, indicating the trivial default constructor should be provided by the compiler. Trivial implementations can be also provided for the _copy constructor_, _assignment operator_ and the _destructor_. Some of them may be prohibited through the `= delete` syntax.

```cpp
class Strings {
    public:
        Strings() = default;
        Strings(std::string const *sp, size_t size);
        
        Strings(Strings const &other) = delete;
}
```

## Constness

The implementation of a `const` member function must be repeated.

```cpp
string const &Person::name() const 
{
//...
}
```

If there are overloaded functions with `const` and without `const`, 

- when the object is a `const` object, only `const` member functions can be used;

- when the object is not a `const` object, the non-const member functionhs are used unless only a `const` member function is available.

```cpp
#include <iostream>
using namespace std;
class Members
{
public:
    Members();
    void member();
    void member() const;
};

Members::Members()
{}

void Members::member()
{
    cout << "non const member\n";
}

void Members::member() const
{
    cout << "const member\n";
}

int main()
{
    Members const constObject;
    Members
    nonConstObject;
    
    constObject.member();
    nonConstObject.member();
}
```

```bash
const member
non const member
```

member functions should always be given the const attribute, unless they actually modify the object’s data.

Anonymous objects can be used:

- to initialize a function parameter which is a `const` reference to an object;

- if the object is only used inside the function call.

Anonymous objects used to initialize const references should not be confused with passing anonymous objects to parameters defined as rvalue refrence. The lifetime of anonymous objects are limited to the statements, rather than the end of the block in which they are defined.

## `inline`

`inline` is a request to the compiler: the compiler may decide to ignore it, and will probably ignore it when the function's body contains much code.

In general, inline functions should not be used. Defining inline functions may be considered when they consist of one very simple statement. The following code involving I/O operations takes a relatively long time, where inlining makes no difference.

```cpp
inline void Person::printname() const
{
    cout << d_name << '\n';
}
```

All sources using a inline functions must be recompiled if the inline function is modified.

Virtual functions should never be defined inline and always out-of-line.

### (C++17) inline variable

The same rules for inline functions are applied to inline variables. `inline` is applicable to variables only with static storage duration (`static` or namespace scope variables). `inline` variables eliminate the main obstacle to packaging C++ code as header-only libraries.
