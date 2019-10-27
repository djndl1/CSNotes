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

To prevent [circular dependency](https://stackoverflow.com/questions/625799/resolve-build-errors-due-to-circular-dependency-amongst-classes), use forward class reference before the class interface and include the needed header after using.

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

# `static` 

Common to all objects of a class.

## `static` data

`static` data is created and initialized only once. They are created as soon as the program starts. `static` data are not initialized by constructors. At most they are modified. It can be defined and initialized in a source file. In the class interface, they are only declared.

```cpp
#include "myheaders.h"

char Directory::s_path[200] = "/usr/local";
```

```cpp
// an interface connecting to a display device
class Graphics
{
    static int s_nobjects;

    public:
        Graphics();
        ~Graphics();
private:
        void setgraphicsmode();
        void settextmode();
}

int Graphics::s_nobjects = 0;
Graphics::Graphics()
{
    if (!s_nobjects++)
        setgraphicsmode(); // set the device to graphic mode when the first graphic interface is initialized
}
Graphics::~Graphics()
{
    if (!--s_nobjects)
        settextmode();
}
```

`static const` data members should be initialized like any other static data member: in source files defining these data members (better always so). In-class initialization may be possible (although not strictly required for compilers) of built-in primitive data types. In-class initialization of integer constatn values is possible using enums.

```cpp
class X {
public:
    enum { s_x = 34 };
    enum: size_t { s_maxWidth = 100 };
}
```

### Generalized constant expressions (`constexpr`)

Generalized `const` expressions can be used as an alternative to C macro function.

`constexpr` can only be applied to definitions. Variables defined with the `constexpr` modifier have constant values. Moreover, it can be applied to functions. A `constexpr` specifier used in an object declaration or non-static member function (until C++14) implies `const`.

#### `constexpr` functions

A constant expression functions has the following characteristics:

- it returns a `constexpr` modified value and consists of only a single return statement.

- it is implicitly declared `inline`.

Such functions are also called _na med constant expression with parameters_. If they are called with compile-time evaluated arguments then the returned value is considered a `const` value as well. It's an encapsulation of expressions. If the arguments cannot be evaluated at compile time, the return values are no longer considered constant expressions and the function behaves like any other function.

In situations where `static const` member data must be accessed, a `constexpr` function can be used as an accessor.

```cpp
class Data
{
    static size_t const s_size = 7;
public:
    static size_t constexpr size();
    size_t constexpr mSize();
};

size_t constexpr Data::size()
{
    return s_size;
}

size_t constexpr Data::mSize()
{
    return size();
}

double data[ Data::size() ];
short data2[ Data().mSize() ];
```

C++14 has relaxed requirements for `constexpr` functions. TODO

#### `constexpr` data

Constant expression class-type objects must be initialized with constant expression arguments; the constructor that is actually used must itself have been declared with the `constexpr` modifier, whose member initializers only use constant expressions and whose body is empty.

An object constructed with a constant-expression constructor is called a _user-defined literal_. Destructors
and copy constructors of user-defined literals must be trivial.

### `static` member functions

`static` member functions can access all static members of their class, but also the members of objects of thpeir class if they are informed about the existence of these objects. A `static` member function is completely comparable to a global function, not associated with any class. The C++ standard does not prescribe the same calling conventions for static member functions as for classless global functions. In practice, the calling conventions are identical, meaning that the address of a static member function could be used as an argument of functions having parameters that are pointers to global functions. It is suggested to create global classless wrapper functions around static member functions that must be used as callback functions for other functions.

However, traditional situations in which call back functions are used in C are tackled in C++ using template algorithms


# Classes and Memory Allocation

## `new` and `delete`

`new` is type safe. It knows about the type of allocated entity it may and will call the constructor of an allocated class type object. When confronted with failing memory allocation, `new`'s behavior is configurable through the use of a `new_handler`.

All `malloc` and `str...` in C should be deprecated in favor of `string`, `new` and `delete`.

`new` uses a type as its operand, which guarantees the correct amount of memory being allocated. `delete` can safely operate on a `NULL` pointer (It's not that `delete` guarantees this, it's `free` that does this).

```cpp
//from libstdc++
_GLIBCXX_WEAK_DEFINITION void
operator delete(void* ptr) _GLIBCXX_USE_NOEXCEPT
{
  std::free(ptr);
}
```

```c
// from musl libc
void free(void *p)
{
	if (!p) return;

    //...
}
```

About two different `new`, see 

- [How is the C++ new operator implemented](https://stackoverflow.com/questions/9595758/how-is-the-c-new-operator-implemented)

- [operator new and new expression](https://stackoverflow.com/questions/1885849/difference-between-new-operator-and-operator-new)

POD types without constructors are not guaranteed to initialized to zero unless adding the brackets `()`. If the struct has a default data member initializer, `()` initializes the POD data to that. Objects of arrays are initialized using their constructors (with the default constructors only).

It's totally legal and safe to create `new int[0]` (and `malloc(0)`, both of which returns nonzero pointers under glibc and musl).

When calling `delete`, the class's destructor is called and the memory pointed at by the pointer is returned to the common pool.

```cpp
string **sp = new string *p[5];
for (size_t idx = 0; idx != 5; ++idx)
    sp[idx] = new string;
delete[] sp; //memory leak
```

Static and local arrays cannot be resized. Resizing is only possible for dynamically allocated arrays.

```cpp
string *enlarge(string *old, size_t oldsize size_t newsize)
{
    string *tmp = new string[newsize];
    for (size_t idx = 0; idx != oldsize; ++idx) {
        tmp[idx] = old[idx];
    }

    delete[] old;
    return tmp;
}
```

## Managing raw memory

Raw memory is made available by `operator new(sizeInBytes)` and also by `operator new[](sizeInBytes)`. They have no concept of data types the size of the intended data type must be specified. The counterparts are `operator delete()` and `operator delete[]()`.

```cpp
inline _LIBCPP_INLINE_VISIBILITY void *__libcpp_allocate(size_t __size, size_t __align) {
#ifndef _LIBCPP_HAS_NO_ALIGNED_ALLOCATION
  if (__is_overaligned_for_new(__align)) {
    const align_val_t __align_val = static_cast<align_val_t>(__align);
# ifdef _LIBCPP_HAS_NO_BUILTIN_OVERLOADED_OPERATOR_NEW_DELETE
    return ::operator new(__size, __align_val);
# else
    return __builtin_operator_new(__size, __align_val);
# endif
  }
#else
  ((void)__align); // https://stackoverflow.com/questions/34288844/what-does-casting-to-void-really-do
#endif
#ifdef _LIBCPP_HAS_NO_BUILTIN_OPERATOR_NEW_DELETE
  return ::operator new(__size);
#else
  return __builtin_operator_new(__size);
#endif
}

_LIBCPP_NODISCARD_AFTER_CXX17 _LIBCPP_INLINE_VISIBILITY
    pointer allocate(size_type __n, allocator<void>::const_pointer = 0)
        {
        if (__n > max_size())
            __throw_length_error("allocator<T>::allocate(size_t n)"
                                 " 'n' exceeds maximum supported size");
        return static_cast<pointer>(_VSTD::__libcpp_allocate(__n * sizeof(_Tp), _LIBCPP_ALIGNOF(_Tp)));
        }
```

```cpp
_LIBCPP_INLINE_VISIBILITY void deallocate(pointer __p, size_type __n) _NOEXCEPT
        {_VSTD::__libcpp_deallocate((void*)__p, __n * sizeof(_Tp), _LIBCPP_ALIGNOF(_Tp));}
        
inline _LIBCPP_INLINE_VISIBILITY void __libcpp_deallocate(void* __ptr, size_t __size, size_t __align) {
  _DeallocateCaller::__do_deallocate_handle_size_align(__ptr, __size, __align);
}

  void __do_deallocate_handle_size_align(void *__ptr, size_t __size, size_t __align) {
#if defined(_LIBCPP_HAS_NO_ALIGNED_ALLOCATION)
    ((void)__align);
    return __do_deallocate_handle_size(__ptr, __size);
#else
    if (__is_overaligned_for_new(__align)) {
      const align_val_t __align_val = static_cast<align_val_t>(__align);
      return __do_deallocate_handle_size(__ptr, __size, __align_val);
    } else {
      return __do_deallocate_handle_size(__ptr, __size);
    }
#endif
  }
  
  static inline void __do_deallocate_handle_size(void *__ptr, size_t __size) {
#ifdef _LIBCPP_HAS_NO_SIZED_DEALLOCATION
    ((void)__size);
    return __do_call(__ptr);
#else
    return __do_call(__ptr, __size);
#endif
  }
  
  template <class _A1>
  static inline void __do_call(void *__ptr, _A1 __a1) {
#if defined(_LIBCPP_HAS_NO_BUILTIN_OPERATOR_NEW_DELETE) || \
    defined(_LIBCPP_HAS_NO_BUILTIN_OVERLOADED_OPERATOR_NEW_DELETE)
    return ::operator delete(__ptr, __a1);
#else
    return __builtin_operator_delete(__ptr, __a1);
#endif
  }

  static inline void __do_call(void *__ptr) {
#ifdef _LIBCPP_HAS_NO_BUILTIN_OPERATOR_NEW_DELETE
    return ::operator delete(__ptr);
#else
    return __builtin_operator_delete(__ptr);
#endif
  }  
```

## the placement `new` operator

Placement `new` is declared in `<memory>` header. Placement `new` is passed an existing block of memory into which `new` initializes an object or value (placing the object in a certain place in memory). 

```cpp
type *new(void *memory) type{arguments};
```

The placement `new` operator is useful in situations where classes set aside memory to be used later(e.g. `std::vector` allocate more memory than it currently needs).

```cpp
template <class _Tp>
class _LIBCPP_TEMPLATE_VIS allocator
{
// ...
template <class _Up, class... _Args>
_LIBCPP_INLINE_VISIBILITY
void
construct(_Up* __p, _Args&&... __args)
{
            ::new((void*)__p) _Up(_VSTD::forward<_Args>(__args)...);
}
//...
}
```

Memory allocated by objects initialized using placement new is returned by explicitly calling the object’s destructor.

```cpp
void Strings::destroy()
{
    for (std::string *sp = d_memory + d_size; sp-- != d_memory; )
        sp->~string();
        
    operator delete(d_memory);
}
```

## The Destructor
The destructors of dynamically allocated objects are not automatically activated and when a program is interrupted by an `exit` call, destructors of locally defined objects by functions are not called, only globally initialized objects are called (which is a good reason why C++ should avoid `exit()`).

A destructor's main task is to ensure that memory allocated by an object is properly returned when the object ceases to exist.

Destructors are only called for fully constructed objects (at least one of its constructors normally completes). Destructors are called:

- destructors of static or global objects are called when the program itself terminates;

- when a dynamically allocated object or arrayis `delete`d;

- when explicitly called;

- destructors of local non-static objects are called automatically when th execution flow leaves the _block in which they are defined_; the destructors of objects defined in the outer block of a function are called just before the function terminates.

One of the advantage of the operators `new` and `delete` over functions like `malloc` and `free` is that they call the corresponding object constructors and destructors. However, the pointer returned by `new` and `new type[]` is indistinguishable. `delete`ing an array of objects allocated by `new type[]` only destroys the first one. Conversely, `delete[]` an object allocated by `new` may cause the program to crash.

The C++ run-time system ensures that when memory allocation fails an error function is activated. By default it throws a `bad_alloc` exception, terminating the program, thus no need to check the return value of `new`. The handler can be defined by users using `set_new_handler()`.

## The assignment operator

In C++, struct and class type objects can be directly assigned new values in the same way as in C. The default action of such an assignment for non-class type data members is a straight byte-by-byte copy from one data member to another.

Operator overloading should be used in situations whre an operator has a defiend action but this default action has undesired side effects in a given context. It should be commonly applied and no surprise is introduced when it's redefined.

Operator overloaded can be used explicitly and must be used explictly when you want to call the overloaded operator from a pointer to an object.

```cpp
Person *tmp = new Person
```

### `this` pointer

A member function of a given class is always called in combination with an object of its class. There
is always an implicit ‘substrate’ for the function to act on. C++ defines a keyword, `this`, to reach this
substrate. The `this` pointer is implicitly declared by every member function. 

A overloaded assignment operator should return `*this`. 

Overloaded operators may themseles be overloaded. 

```cpp
// in std::string
operator=(std::string const &rhs);
operator=(char const *rhs);
...
```

### Copy Constructor

Besides explicit copy construction, copy constructors are called when pass by value or return by value.

```cpp
String copy(Strings const &store)
{
    return store; // a temporary `Strings`' object is constructed.
}
```

More at [return value and constructors](https://stackoverflow.com/questions/13430831/should-i-return-an-rvalue-reference-by-stdmoveing) and copy elision
 [Should I return by rvalue reference](https://stackoverflow.com/questions/29332516/return-rvalue-reference-vs-return-by-value-in-function-return-type)
 [Member function ref-qualifier](https://stackoverflow.com/questions/8610571/what-is-rvalue-reference-for-this)

### Swapping

The copy assigment may be implemented generically as

```cpp
void Strings::swap(Strings &other)
{
    swap(d_string, other.d_string);
    swap(d_size, other.d_size);
}

Strings &operator=(Strings const &other)
{
    Strings tmp{other};
    swap(tmp);
    return *this;
}
```

Or more concisely using move-assignment.

```cpp
Class &operator=(Class const &other)
{
    Class tmp{ other };
    return *this = std::move(tmp);
}
```

Many classes offer `swap` members allowing to swap two of their objects. STL offers variaous functions related to swapping and a generic `std::swap`.

When implementing a `swap` member function, it is not always a good idea to swap every data member of a class, like when in a linked list or a data member referring/pointing to another data member in the same object. Simple swapping operations must be avoided when data members point or refer to data that is involved in the swapping.

Sometimes, a `swap` implementation using `memcpy` can be fast (this is barbarous!):

```cpp
#include <cstring>
void Class::swap(Class &other)
{
    char buffer[sizeof(Class)];
    memcpy(buffer, &other, sizeof(Class));
    memcpy(reinterpret_cast<char *>(&other), this, sizeof(Class));
    memcpy(reinterpret_cast<char *>(this), buffer, sizeof(Class));
}
```

## Move Semantics

Moving information is based on the concept of anonymous data and in general by functions returning their results by value instead of returning references or pointers. Anonymous values are always short-lived.

Classes supporting move operations like move assignment and move constructors are called _move-aware_.

A rvalue reference only binds to an anonymous temporary value.  The compiler is required to call functions offering movable parameters whenever possible. Once a temporary value has a name, it is no longer an anonymous temporary value and within such functions the compiler no longer calls functions expecting anonymous temporary values when the parameters are used as arguments.

```cpp
#include <iostream>
using namespace std;

class Class {
public:
    Class() {}

    void fun(Class const &other)
        {
            cout << "fun: Class const &\n";
            gun(other);
        }

    void fun(Class &other)
        {
            cout << "fun: Class &\n";
            gun(other);
        }

    void fun(Class &&other)
        {
            cout << "fun: Class &&\n";
            gun(other);
        }

    void gun(Class const &other)
        {
            cout << "gun: class const &\n";
        }

    void gun(Class &other)
        {
            cout << "gun: class &\n";
        }

    void gun(Class &&other)
        {
            cout << "gun: class &&\n";
        }
};

int main()
{
    Class c1;

    c1.fun(c1);
    c1.fun(Class{});
    Class const c0;
    c1.fun(c0);
}
```

```bash
 djn  debian  ~/FOSS/playground  ./a.out 
fun: Class &
gun: class &
fun: Class &&
gun: class &
fun: Class const &
gun: class const &
```

Generally it is pointless to define a function having an rvalue reference return type. It may causes a dangling reference.

```cpp
std::string &&doubleString(std::string &&tmp)
{
    tmp += tmp;
    return std::move(tmp);
}

std::cout << doubleString(std::string("hello "));
```

### The move constructor

Move constructors of classes using dynamic memory allocation are allowed to assign the values of pointer data members to their own pointer data members without requiring them to make a copy of the source’s data.  Next, the temporary’s pointer value is set to zero to prevent its destructor from destroying data now owned by the just constructed object. The move constructor has grabbed or stolen the data from the temporary object.

The class benefits from move operations when one or more of the composed data members themselves support move operations. Move operations cannot be implemented if the class type of a composed data member does not support moving or copying. Currently, `stream` classes fall into this category.

```cpp
Person::Person(Person &&tmp) : d_name( std::move(tmp.d_name) ), d_address( std::move(tmp.d_address) )
{}
```

Having available a rvalue does not mean that we're referring to an anonymous object, so `std::move` is required.

When a class using composition not only contains class type data members but also other types of data
(pointers, references, primitive data types), then these other data types can be initialized as usual.
Primitive data type members can simply be copied; references and pointers can be initialized as usual (just copy-initialized).

### Move-assignment

In addition to the overloaded assignment operator a move assignment operator may be implemented for classes supporting move operations. In this case, if the class supports swapping the implementation is surprisingly simple.

```cpp
Class &operator=(Class &&tmp)
{
    swap(tmp);
    return *this;
}
```

If swapping is not supported then the assignment can be performed for each of the data members in turn, using `std::move`.

```cpp
Person &operator=(Person &&tmp)
{
    d_name = std::move(tmp.d_name);
    d_address = std::move(tmp.d_address);
    return *this;
}
```

### Moving and the destructor
 
When moving pointer values from a temporary source to a destination the move constructor should make sure that the temporary’s pointer value is set to zero, to prevent doubly freeing memory. Primitive types should also be set to zero since they might be used in destructors.

read [why set primitive type to zero move semantics](https://stackoverflow.com/questions/33470156/why-set-primitive-type-value-to-zero-in-the-end-of-move-constructor-function)

### Default move constructors and assignment operators

- If the copy constructor or the copy assignment operator is declared, then the default move constructor and move assignment operator are suppressed;

- If the move constructor or the move assignment operator is declared then the copy constructor and the copy assignment operator are implicitly declared as deleted, and can therefore not be used anymore;

- If either the move constructor or the move assignment operator is declared, then (in addition to suppressing the copy operations) the default implementation of the other move-member is also suppressed;

- In all other cases the default copy and move constructors and the default copy and assignment operators are provided.

If default implementations of copy or move constructors or assignment operators are suppressed, add them back and append `= default`.

### Moving: design

For classes offering value sematics (able to initialize/be assigned to objects of their classes).

- Classes using pointers to dynamically allocated memory, owned by the class’s objects must be provided with a copy constructor, an overloaded copy assignment operator and a destructor;

- Classes using pointers to dynamically allocated memory, owned by the class’s objects, should be provided with a move constructor and a move assignment operator;

-  The copy- and move constructors must always be implemented independently from each other.

Whenever a member of a class receives a `const&` to an object of its own class and creates a copy of that object to perform its actual actions on, then that function’s implementation can be implemented by an overloaded function expecting an rvalue reference. e.g. implementing a copy assginment in terms of move assignment.

## Copy Elision and Return Value Optimization

### How a member function is selected

For a named argument:

|       | ono-const | const |
| :---: | :---:     | :---: |
|       | (T&)       |       |
|       | (T const &) | (T const &) |

For an anonymous argument

|   | non-const | const |
|---|-----------|-------|
|   | (T&&)     | )     |
|   | (T const &) | (T const &) |

A function with value parameters and another overloaded function with reference parameters cause the compiler to report an ambiguity error. All arguments can be used with a function specifying a `T const &` parameter. For anonymous arguments, a similar catchall is available having a higher priority: `T const &&`. A function like this cannot modify the parameter but copy it. This kind of function should be replaced by functions that have `T const &` when accepting anonymous objects.`

The compiler can choose to avoid making copies (copy elision/return value optimization). All modern compilers apply copy elision.

- if a copy or move constructor exists, try copy elision.

- if a move constructor exists, move

- if a copy constructor exists, copy

- report an error

```cpp
#include <utility>
#include <iostream>
#include <vector>
 
struct Noisy {
    Noisy() { std::cout << "constructed\n"; }
    Noisy(const Noisy&) { std::cout << "copy-constructed\n"; }
    Noisy(Noisy&&) { std::cout << "move-constructed\n"; }
    ~Noisy() { std::cout << "destructed\n"; }
};
 
std::vector<Noisy> f() {
    std::vector<Noisy> v = std::vector<Noisy>(3); // copy elision when initializing v
                                                  // from a temporary (until C++17)
                                                  // from a prvalue (since C++17)
    return v; // NRVO from v to the result object (not guaranteed, even in C++17)
}             // if optimization is disabled, the move constructor is called
 
void g(std::vector<Noisy> arg) {
    std::cout << "arg.size() = " << arg.size() << '\n';
}
 
int main() {
    std::vector<Noisy> v = f(); // copy elision in initialization of v
                                // from the temporary returned by f() (until C++17)
                                // from the prvalue f() (since C++17)
    g(f());                     // copy elision in initialization of the parameter of g()
                                // from the temporary returned by f() (until C++17)
                                // from the prvalue f() (since C++17)
}
```

```bash
constructed
constructed
constructed
constructed
constructed
constructed
arg.size() = 3
destructed
destructed
destructed
destructed
destructed
destructed
```

## Unrestricted Unions

unrestricted unions allow addition of data fields of types for which non-trivial constructors were defined. Such data fields commonly are of class types.

TODO

## Aggregate Data Types

Structs are still used in C++, mainly to store and pass around aggregates of different data types. A commonly used term for these structs is aggregate (in some languages known as plain old data (pod)). Aggregates are commonly used in C++ programs to merely combine data in dedicated (struct) types. Some members (constructors, destructors, overloaded assignment operator) may implicitly be defined.

Aggregates should not have user provided special member functions, virtual members. Aggregates should inherit only publicly and the base classes aren't not virtual. Its non-static members have public access rights.


# Inheritance/derivation

_is-a_ or _is-implemented-in-terms-of_.

A rule of thumb for choosing between inheritance and composition distinguishes _is-a_ and _has-a_ relationships.

As a rule of thumb, derived classes must be fully recompiled (but don’t have to be modified) when the data organization (i.e., the data members) of their base classes change. Adding new member functions to the base class doesn’t alter the data organization so no recompilation is needed when new member functions are added (virtual member functions excluded).

Repeatedly deriving classes from classes quickly results in big, complex class hierarchies that are hard to understand, hard to use and hard to maintain. Hard to understand and use as users of our derived class now also have to learn all its (indirect) base class features as well. Hard to maintain because all those classes are very closely coupled. When designing classes always aim at the lowest possible coupling. Big class hierarchies usually indicate poor understanding of robust class design.

Often classes can be defined in-terms-of existing classes: some of their features are used, but others need to be shielded off.

Avoid the temptation to declare data members in a class’s protected section: it’s a sure sign of bad class design as it needlessly results in tight coupling of base and derived classes. If a derived class (but not other parts of the software) should be given access to its base class’s data, use member functions: accessors and modifiers declared in the base class’s protected section.



## Public, protected and private derivation

When protected derivation is used all the base class’s public and protected members become protected members in the derived class. Classes that are in turn derived from the derived class view the base class’s members as protected.

When private derivation is used all the base class’s members turn into private members in the derived class. The derived class members may access all base class public and protected members but base class members cannot be used elsewhere.

Public derivation should be used to define an is-a relationship between a derived class and a base class: the derived class object is-a base class object allowing the derived class object to be used poly- morphically as a base class object in code expecting a base class object. Private inheritance is used in situations where a derived class object is defined in-terms-of the base class where composition can- not be used. There’s little documented use for protected inheritance, but one could maybe encounter protected inheritance when defining a base class that is itself a derived class making its base class members available to classes derived from it.

Combinations of inheritance types do occur.

When private or protected derivation is used, users of derived class objects are denied access to the base class members. Private derivation denies access to all base class members to users of the derived class, protected derivation does the same, but allows classes that are in turn derived from the derived class to access the base class’s public and protected members.

Private inheritance should be used when deriving a class `Derived` from `Base` where `Derived` is-implemented-in-terms-of Base. In general terms composition results in looser coupling and should therefore be preferred over inheritance. Protected inheritance may be considered when the derived class (D) itself is intended as a base class that should only make the members of its own base class (B) available to classes that are derived from it (i.e., D).

TODO

### promoting acess rights

 Access promotion allows us to specify which members of private (or protected) base classes become available in the protected (or public) interface of the derived class.

```cpp
class RandStream : private RandBuf, public std::istream { // RandBuf derived from std::streambuf
    public:
        using std::streambuf::in_avail; 
}
```

Another way is to define a shadow member

```cpp
class RandStream: private RandBuf, public std::istream
{
    // implements a stream to extract random values from
    public:
    std::streamsize in_avail();
};
inline std::streamsize RandStream::in_avail()
{
    return std::streambuf::in_avail();
}
```

## Special Member Functions

The base class must have been constructed before the actual derived class elements can be initialized. 

- When constructing a derived class object a base class constructor is always called before any action is performed on the derived class object itself. By default the base class’s default constructor is going to be called.

- Using the base class constructor only to reassign new values to its data members in the derived class constructor’s body usually is inefficient. In those cases a specialized base class constructor must be used instead of the base class default constructor.

Calling a base class constructor in a constructor’s initializer clause is called a base class initializer. The base class initializer must be called before initializing any of the derived class’s data members and when using the base class initializer none of the derived class data members may be used.

A move constructor for a derived class whose base class is move-aware must anonymize the rvalue reference before passing it to the base class move constructor.

```cpp
Car &Car::operator=(Car &&tmp)
{
    static_cast<Land &>(*this) = std::move(tmp);
    // move Car's own data members next
    return *this;
}
```

Derived classes can be constructed without explicitly defining derived class constructors.

Derived classes may redefine base class members, which will shadow the one from the base class. To use the base definition, call it explicitly.

```cpp
void Truck::setMass(size_t tractor_mass, size_t trailer_mass)
{
    d_mass = tractor_mass + trailer_mass;
    Car::setMass(tractor_mass);
     // note: Car:: is required
     
}

void Truck::setMass(size_t tractor_mass, size_t trailer_mass)
{
        d_mass = tractor_mass + trailer_mass;
        Car::setMass(tractor_mass);
        // note: Car:: is required
}

```

To prevent hiding the base class members a using declaration may be added to the derived class interface.  This prevents non-member from using `Car::setMass` without scope resolution.

```cpp
class Truck: public Car
{
public:
    using Car::setMass;
    void setMass(size_t tractor_mass, size_t trailer_mass);
};
```

## Multiple Inheritance

When using multiple inheritance it should be defensible to consider the newly derived class an instantiation of both base classes. Otherwise, composition is more appropriate.  In C++ there are various good arguments for using multiple inheritance as well, without violating the ‘one class, one responsibility’ principle.

```cpp
class NavSet {
public:
    NavSet(Intercom &intercom, VHF_Dial &dial);

    size_t activeFrequency() const;
    size_t standByFrequency() const;

    void setStandByFrequency(size_t freq);
    size_t toggleActiveStandBy();
    void setVolume(size_t level);
    void identEmphasis(bool on_off);
};

class ComSet {
public:
    ComSet(Intercom &intercom);

    size_t frequency() const;
    size_t passiveFrequency() const;

    void setPassiveFrequency(size_t freq);
    size_t toggleFrequenciezs();

    void setAudioLevel(size_t level);
    void powerOn(bool on_off);
    void testState(bool on_off);
    void transmit(Message &msg);
};
```

```cpp
class NavComSet : public ComSet, public NavSet {
public:
    NavComSet(Intercom &intecom, VHF_dial &dial) :
    ComSet(intercom), NavSet(intercom, dial)
    {}

```

In situations where two base classes offer identically named members special provisions need to be made to prevent ambiguity:

- The intended base class can explicitly be specified using the base class name and scope resolution operator;

-  If the NavComSet class is obtained from a third party, and cannot be modified, a disambiguating wrapper class may be used;

- The class interface is provided with member functions that can be called unambiguously (wrappers around conflicting base class member functions). These additional members are usually defined inline.

The same base class might be inherited multiple times, causing ambiguity.

## Conversions between base classes and derived classes

When assigning a base class object from a derived class object only the base class data members are assigned, other data members are dropped, a phenomenon called _slicing_. 

In assignments in which base class objects and derived class objects are involved, assignments in which data are dropped are legal (called slicing). Assignments in which data remain unspecified are not allowed. Of course, it is possible to overload an assignment operator to allow the assignment of a derived class object from a base class object.

```cpp
Land land(1200, 130);
Car car(500, 75, "Daf");
Truck truck(2600, 120, "Mercedes", 6000);
Vehicle *vp;

vp = &land;
vp = &car;
vp = &truck;
```

When using `vp` only the member functions manipulating mass can be called as this is the Vehicle’s only functionality.  When a function is called using a pointer to an object, then the type of the pointer (and not the type of the object) determines which member functions are available and can be executed. If the actual type of the object pointed to by a pointer is known, an explicit type cast can be used to access the full set of member functions that are available for the object.

## Using non-default constructor with `new[]`

Usually 

```cpp
string *sp = new string[10];
fill(sp, sp + 10, string("hello world"));
```

Inheritance can be used to call non-default constructors in combination with operator `new[]`

```cpp
namespace {
struct Xstr : public string {
    Xstr() : string{"hello world"} {}
};
}mbt

string *sp = new Xstr[10];
```

TODO


# Polymorphism

_Liskov Substitution Principle_ (???)
 
 LSP is implemented using polymorphism.  Polymorphism allows us to reimplement members of base classes and to use those reimplemented members in code expecting base class references or pointers. Using polymorphism existing code may be reused by derived classes reimplementing the appropriate members of their base classes. Reusability is enhanced if we add a redefinable interface to the base class’s interface. A redefinable interface allows derived classes to fill in their own implementation, without affecting the user interface. At the same time the user interface will behave according to the derived class’s wishes, and not just to the base class’s default implementation. Members of the reusable interface can be declared in the class’s private sections: conceptually they merely belong to their own classes. Separating the user interface from the redefinable interface is a sensible thing to do. It allows us to fine-tune the user interface (only one point of maintenance), while at the same time allowing us to standardize the expected behavior of the members of the redefinable interface.
 
 C++ uses early binding and offers both dynamic and static dispatch. [What the hell are those binding](https://softwareengineering.stackexchange.com/questions/387697/what-is-late-binding) 

The following code does not result in duck typing

```cpp
class Base
{
protected:
    void hello()
    {
        cout << "base hello\n";
    }
public:
    void process()
    {
        hello();
    }
};

class Derived: public Base
{
protected:
    void hello()
    {
        cout << "derived hello\n";
    }
};

int main()
{
    Derived derived;
    derived.process();
}
```
 
since `process()` is statically compiled against `Base::hello()`.

The keyword `virtual` should not be mentioned for members in derived classes which are declared virtual in base classes. In derived classes those members should be provided with the `override` indicator, allowing the compiler to verify that you’re indeed referring to an existing virtual member function. 

Destructors should _always be defined_ `virtual` in classes designed as a base class from which other classes are going to be derived. If a class is polymorphic (declares or inherits at least one virtual function), and its destructor is not virtual, deleting it is undefined behavior regardless of whether there are resources that would be leaked if the derived destructor is not invoked.

The identifier `final` can be applied to class declarations to indicate that the class cannot be used as a base class. The identifier `final` can also be added to virtual member declarations, indicating that those virtual members cannot be overridden by derived classes.

Virtual functions should never be implemented inline. Since the vtable contains the addresses of the class’s virtual functions, these functions must have addresses and so they must have been compiled as real (out-of-line) functions.

The notion of a virtual constructor is not supported.

## Pure Virtual

Virtual member functions do not necessarily have to be implemented in base classes. Abstract base classes are the foundation of many design patterns, allowing the programmer to create highly reusable software. Members that are merely declared in base classes are called pure virtual functions. A virtual member becomes a pure virtual member by postfixing `= 0` to its declaration. Pure virtual member functions may be implemented. Implementing a pure virtual member has limited use. 

[About ctors and dtors constructor]](https://stackoverflow.com/questions/14184341/c-constructor-destructor-inheritance)

[Pure virtual destructor](https://stackoverflow.com/questions/1219607/why-do-we-need-a-pure-virtual-destructor-in-c). However, a destructor of a derived class implicitly calls the destructor of its base class, even if that destructor is pure virtual. A pure virtual destructor must have an implementation.

[About data members in an abstract class](https://stackoverflow.com/questions/1193138/virtual-base-class-data-members)



Polymorphism can also be used in combination with multiple inheritance. To avoid ambiguity when inheriting a base class multiple times, use virtual base classes

## Virtual Inheritance

```cpp
struct B { int n; };
class X : public virtual B {};
class Y : virtual public B {};
class Z : public B {};
// every object of type AA has one X, one Y, one Z, and two B's:
// one that is the base of Z and one that is shared by X and Y
struct AA : X, Y, Z {
    AA() {
        X::n = 1; // modifies the virtual B subobject's member
        Y::n = 2; // modifies the same virtual B subobject's member
        Z::n = 3; // modifies the non-virtual B subobject's member
 
        std::cout << X::n << Y::n << Z::n << '\n'; // prints 223
    }
};
```

```bash
223
```

An example of an inheritance hierarchy with virtual base classes is the iostreams hierarchy of the standard library: `std::istream` and `std::ostream` are derived from `std::ios` using virtual inheritance. `std::iostream` is derived from both `std::istream` and `std::ostream`, so every instance of `std::iostream` contains a `std::ostream` subobject, a `std::istream` subobject, and just one `std::ios` subobject (and, consequently, one `std::ios_base`).

Virtual derivation is, in contrast to virtual functions, a pure compile-time issue. Virtual inheritance merely defines how the compiler defines a class’s data organization and construction process.

## Run-time type identification

C++ offers run-time type identification through the dynamic cast and typeid operators.

The `dynamic_cast<>` operator is used to convert a base class pointer or reference to, respectively, a derived class pointer or reference. A dynamic cast’s actions are determined run-time; it can only be used if the base class declares at least one virtual member function. The cast fails and returns 0 (if a dynamic cast of a pointer was requested) or throws a `std::bad_cast` exception (if a dynamic cast of a reference was requested).

We could determine the actual class of an object a pointer points to by performing a series of checks to find the derived class to which a base class pointer points.


A dynamic cast is a cast, and casts should be avoided whenever possible. When using dynamic casts in your own code always properly document why the
dynamic cast was appropriately used and was not avoided.

As with the dynamic_cast operator, typeid is usually applied to references to base class objects that refer to derived class objects. Typeid should only be used with base classes offering virtual members. The typeid operator returns an object of type `type_info`. The typeid operator can be used to determine the name of the actual type of expressions, not just of class type objects.

examples TODO

## Implementation

when multiple inheritance is used (each base class defining virtual members) another approach is followed to determine which virtual function to call, the class Derived receives two vtables, one for each of its base classes and each Derived class object harbors two hidden vpointers, each one pointing to its corresponding vtable.

## Prototype Design Pattern

TODO

# Friends

By using the friend keyword functions are granted access to a class’s private members. Even so, this does not imply that the principle of data hiding is abandoned when the friend keyword is used. Friend declarations are true declarations. Once a class contains friend declarations these friend functions do not have to be declared again below the class’s interface. This also clearly indicates the class designer’s intent: the friend functions are declared by the class, and can thus be considered functions belonging to the class.

# Classes Having Pointers to Members

Pointers to members can profitably be used to configure the behavior of objects of classes. Depending on which member a pointer to a member points to objects will show certain behavior.

To define a pointer to a member of a class, the scope of the pointer must indicate class scope, by prefixing the pointer data member by the class name plus scope resolution operator.

```cpp
char const *(String::*d_sp)() const;
```

Pointers to members may be defined in their target classes (so they become data members), or in another class, or as a local variable or as a global variable. The important part is that a pointer to member can be initialized or assigned without requiring the existence of an object of the pointer’s target class. Initializing or assigning an address to such a pointer merely indicates to which member the pointer points. This can be considered some kind of relative address; relative to the object for which the function is called. No object is required when pointers to members are initialized or assigned.

```cpp
#include <cstddef>

class PointerDemo {
public:
    size_t d_value;
    size_t get() const;
};

inline size_t PointerDemo::get() const
{
    return d_value;
}

int main(int argc, char *argv[])
{
    size_t (PointerDemo::*getPtr)() const = &PointerDemo::get;
    size_t PointerDemo::*valuePtr = &PointerDemo::d_value;

    getPtr = &PointerDemo::get;
    valuePtr = &PointerDemo::d_value;
    
    PointerDemo object{};
    PointerDemo *ptr = &object;
    object.*valuePtr = 12345;
    ptr->*valuePtr = 54321;
    return 0;
}
```

Pointers to members can be used profitably in situations where a class has a member that behaves differently depending on a configuration setting.

```cpp
#include <string>

class PersonData {
    static std::string (PersonData::*s_infoPtr[])(Person *p);

    std::string (PersonData::*d_infoPtr)(Person *p);

    PersonData(PersonData::EmployeeCategory cat);

    std::string personInfo(char const *name);

private:
    std::string allInfo(Person *p);
    std::string noPhone(Person *p);
    std::string nameOnly(Person *p);
};

std::string PersonData::personInfo(char const *name)
{
    Person *p = lookup(name);

    return p ? (this->*d_infoPtr)(p) :  "not found";
}

PersonData::PersonData(PersonData::EmployeeCategory cat)
    : d_infoPtr(s_infoPtr[cat])
{}

std::string (PersonData::PersonData::*s_infoPtr[])(Person *p) = {
    &PersonData::allInfo,
    &PersonData::allInfo,
    &PersonData::noPhone,
    &PersonData::nameOnly
};
```

Static members of a class can be used without having available an object of their class. Public static members can be called like free functions, albeit that their class names must be specified when they are called. Since static members have no associated objects their addresses can be stored in ordinary function pointer variables, operating at the global level. Pointers to members cannot be used to store addresses of static members.

```cpp
void fun()
{
    size_t (*pf)() = String::count;
    cout << (*pf)() << '\n';
}
```

Pointers to member function may differ from those of normal pointers in size. 

```cpp
size of pointer to data-member:     8
size of pointer to member function: 16
size of pointer to non-member data: 8
size of pointer to free function:   8
```

With polymorphism and multiple inheritance, the implicit parameter of a non-static member function may not be the address of the object itself, rather plus an offset. When calling a member function, this offset is needed so that the correct implicit parameter is passed. The offset is also stored in the pointer now, requiring extra bits.

```cpp
#include <iostream>

struct A {
    int a;
};

struct B {
    int b;
    void bfun() {}
};

struct C : public A, public B
{

};

int main(int argc, char *argv[])
{
    union BPTR {
        void (B::*ptr)();
        unsigned long value[2];
    } bp;
    bp.ptr = &B::bfun;
    std::cout << std::hex << bp.value[0] << ' ' << bp.value[1] << std::dec << '\n';

    union CPTR {
        void (C::*ptr)();
        unsigned long value[2];
    } cp;
    cp.ptr = &C::bfun;
    std::cout << std::hex << cp.value[0] << ' ' << cp.value[1] << std::dec << '\n';
    return 0;
}
```

```bash
4012e0 0 
4012e0 4 # when calling, (*4012e0)(this + 4, args);
```

# Nested Classes

Classes can be defined inside other classes. Nested classes are used in situation where the nested class has a close conceptual relationship to its surrounding class. e.g. (`string::iterator` inside the class `string`).

The normal access and rules in classes apply to nested classes. If a class is nested in the public section of a class, it is visible outside the surrounding class. If it is nested in the protected section it is visible in subclasses, derived from the surrounding class, if it is nested in the private section, it is only visible for the members of the surrounding class.

To grant the surrounding class access rights to the private members of its nested classes or to grant nested classes access rights to the private members of the surrounding class, the classes can be defined as friend classes.

Nested classes can be considered members of the surrounding class, but members of nested classes are not members of the surrounding class. Nested classes are just typenames. It is not implied that objects of such classes automatically exist in the surrounding class. If a member of the surround- ing class should use a (non-static) member of a nested class then the surrounding class must define a nested class object, which can thereupon be used by the members of the surrounding class to use members of the nested class.

Inline and in-class functions can use any nested class, even if the nested class's definition appears later in the outer class's interface. When (nested) member functions are defined inline, their definitions should be put below their class interface. Static nested data members are also usually defined outside of their classes. 

Nested classes may be declared before they are actually defined in a surrounding class. Such forward declarations are required if a class contains multiple nested classes, and the nested classes contain pointers, references, parameters or return values to objects of the other nested classes.

No friend declaration is required to grant a nested class access to the private members of its surrounding class. A nested class is a type defined by its surrounding class and as such objects of the nested class are members of the outer class and thus can access all the outer class’s members.

```cpp
class Outer {
    int d_value;
    static int s_value;
    
public:
    Outer() : d_value(12) {}
    class Inner {
        public:
            Inner() 
            {
                cout << s_value << '\n';
            }
            Inner(Outer &outer) 
            {
                cout << outer.d_value << '\n';
            }
    };
};
```

Friend declarations may be provided beyond the definition of the entity that is to be considered a friend. So a class can be declared a friend beyond its definition. ???


Nested classes aren’t automatically each other’s friends. Here friend declarations must be provided
to grant one nested classes access to another nested class’s private members.

```cpp
class SecondWithin;

class Surround {
    static int s_variable;
    public:
        class FirstWithin {
            friend class Surround;
            friend class SecondWithin; //a friend declaration is also considered a forward declaration.

            friend class ::SecondWithin;
            
            static int s_variable;
            public:
                int value();
        };
        int value();
    private:
        class SecondWithin {
            friend class Surround;
            friend class FirstWithin;
            
            static int s_variable;
            public:
                int value();
        };
};

inline int Surround::FirstWithin::value()
{
    Surround::s_variable = SecondWithin::s_variable;
    return s_variable;
}

inline int Surround::SecondWithin::value()
{
    Surround::s_variable = FirstWithin::s_variable;
    return s_variable;
}
```

## Nesting Enumerations

Enumerations may also be nested in classes. Nesting enumerations is a good way to show the close connection between the enumeration and its class. Nested enumerations have the same controlled visibility as other class members.

```cpp
class DataStructure {
    public:
        enum Traversal {
            FORWARD;
            BACKWARD;
        };
        setTraversal(Traversal mode);
    private:
        Traversal   d_mode;
};

DaataStructure::Traversal localMode = DataStructure::FORWARD;
```
`enum` types usually define symbolic values. Types may be defined without any associated values. An empty enum can be defined which is an `enum` not defining any values. The empty `enum`’s type name may thereupon be used as a legitimate type in, e.g. a catch clause.