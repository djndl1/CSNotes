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

## Local Classes: classes inside functions

It is entirely possible to define a local classes, inside a function. Local classes can be very useful in advanced applications involving inheritance or templates.

- Local classes cannot define static data members. It is possible to define nested functions in C++.

- Local classes cannot directly access the non-static varaible of their surrounding context. Local classes may directly access global data and static variables defined by their surrounding context.

- Local class objects can be defined inside the function body, but they cannot leave the function as objects of their own type, i.e. not as parameter or return types of its surrounding function.

- A local class may be derived from an existing a class allowing the surrounding function to return a dynamically allocated locally constructed class object, pointer or reference via a base class poointer or reference.

## the keyword `mutable`

In contrast to `const`, C++ also allows the declaration of data members which may be modified, even by const member function or the object itself is `const`.

Mutable sould be used for those data members that may be modified without logically changing the object, which might still be considered a constant object, that is, the externally visible state of the class.

```cpp
mutable char *d_data;

char const *string::c_str() const 
{
    d_data[d_length] = 0; // doesn't really matter to the string 
    return d_data.
}
```

The keyword `mutable` should be sparingly used. Data modified by const member function should never logically modify the object.

```cpp
class ThreadsafeCounter {
  mutable std::mutex m; // The "M&M rule": mutable and mutex go together
  int data = 0;
 public:
  int get() const {
    std::lock_guard<std::mutex> lk(m);
    return data;
  }
  void inc() {
    std::lock_guard<std::mutex> lk(m);
    ++data;
  }
};
```

# Header File Organization

Source files contain the code of member functions of classes. There are two approaches

- All required header files for a member function are included in each individual source file. (Compiler-economy but inconvenient for programmers.)

- All required header files (for all member functions of a class) are include in a header file that is included by each of the source files defining class members. (may contain unnecesary headers )

To prevent (circular dependency)[https://stackoverflow.com/questions/625799/resolve-build-errors-due-to-circular-dependency-amongst-classes], use forward class reference before the class interface and include the needed header after using.

```cpp
#ifndef STRING_H_
#define STRING_H_
class File; // forward reference
class String
{
public:
    void getLine(File &file);
};
#include <project/file.h>  // to know about a File
#endif
```

```cpp
#ifndef FILE_H_
#define FILE_H_
class String; // forward reference
class File
{
public:
    void gets(String &string);
};
#include
<project/string.h> // to know about a String
#endif
```

The above doesn't work with composition (the compiler cannot determine the size of both classes), nor with in-class inline member functions. In such cases, the header files of the classes of the composed objects must have been read before the class interface itself.

- Header files defining a class interface should declare what can be declared before defining the class interface itsefl, that is, base class of the current class, class types of composed data members, inline member functions, which must be known by the compiler before the current class starts. Class types of return values and function parameters do not need their headers before that.

- Program sources in which the class is used only need to include this header file.

- Other additional headers and the class header file can be included in a separate internal header file(`.ih`) in the same directory as the source files of the class.

```cpp
// file.h
#ifndef FILE_H_
#define FILE_H_
#include <fstream> // for composed 'ifstream'
class Buffer;      // forward reference

class File // class interface
{
    std::ifstream d_instream;
public:
    void gets(Buffer &buffer);
};
#endif
```

```cpp
// file.ih
#include <myheaders/file.h> // make the class File known
#include <string> // used by members of the class
#include <sys/stat.h> // File.
#include <buffer.h> // make Buffer known to File
```

No `using` directive should be specified in header files if they are to be used as general header files declaring classes or other entities from a library. As a rule of thumb, header files intended for general use should not contain using declarations. This rule does not hold true for header files which are only included by the sources of a class.

## (C++20) Modules 

TODO
