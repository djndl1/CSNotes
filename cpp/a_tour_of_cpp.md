An overview of C++17

Advice about what constitutes good modern C++ can be found in the C++ Core Guidelines.

# Basics

- A declaration is a statement that introduces an entity into the program. It specifies a type for the entity.

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
At the language level, C++ represents interfaces by declarations.

# Modules (C++20)

`export`ing and `import`ing certain part of a source file as part of the interaface finally become possible.


```c++
//file Vector.cpp

module;
export module Vector;
export class Vector {
// definitions and declarations go here
};

//file user.cpp
import Vector;
#include <cmath>

//...
```

A module is compiled only once; two modules can be imported in either order without chaning their meaning; `import` is not transitive: a module is not imported automatically in a chain of `import`s.

# Error handling

The type system, the higher-level constructs simplify programming, limit opportunities for mistakes and increases the compiler's chances of catching errors.

A function can indicate that it cannot perform its alloted task by 

- throwing an exception

- somehow return a value indicating failure. 

- terminating the program (`terminate()`, `exit()`, `abort()`).

Exception handling is slow; it is often faster than correct handling of complex or rare error conditions and of repeated tests of error codes.

## Exceptions

Exceptions are designed to be used to report failure to complete a given task. Exceptions are integerated with constuctors and destructors to provide a coherent framework for error handling and resource management.

the implementation will unwind the function call stack as needed to get back to the context of that caller. That is, the exception handling mechanism will exit scopes and functions as needed to get back to a caller that has expressed interest in handling that kind of exception, invoking destructors along the way as needed. A function that should never throw an exception can be declared `noexcept`, where any `throw` inside the functions will turn into a `terminate()`.

In well-designed code, `try`-blocks are rare. Avoid overuse by systematically using the RAII technique.

Often, a function has no way of completing its assigned task after an exception is thrown. Then, “handling” an exception means doing some minimal local cleanup and rethrowing the exception.

## Resource Acquisition is Initialization

The basic idea behind RAII is for a constructor to acquire all resources necessary for a class to operate and have the destructor release all resources, thus making resource release guaranteed and implicit.

## Static Assertions

If an error can be found at compile time, it is usuall preferable to do so. The `static_assert` mechanism can be used to for anything that can be expressed in terms of expressions.

```c++
static_assert(4 <= sizeof(int), "integers are too small");
```

The most important uses of `static_assert` come when making assertions about types used as parameters in generic programming.
# Function arguments and Return Values

Key concerns when passing information to and from functions: 

- copied or shared?

- If shared, is it mutable?

- Is an object moved, leaving an empty object behind?

## Argument Passing

When we care about performance, we usually pass small values by-value and larger ones by-reference. Here “small” means “something that’s really cheap to copy.” Exactly what “small” means depends on machine architecture, but “the size of two or three pointers or less” is a good rule of thumb. If we want to pass by reference for performance reasons but don’t need to modify the argument, we pass-by-const-reference.

Using a default argument is sometimes easier than overloading.

## Value return

We return “by reference” only when we want to grant a caller access to something that is not local to the function.

To pass large amounts of information out of a function, use move semantics, i.e. a move constructor or move assignment. Returning large objects by returning a pointer to it is common in older code and a major source of hard-to-find errors. 

## Structured binding

The mechanism for giving local names to members of a class object is called _structured binding_.

```c++
struct Entry {
    string name;
    int value;
};

Entry read_entry(istream& is)
{
    string s;
    int i;
    is >> s >> i;
    return {s, i};
}

auto e = read_entry(cin);
```

```c++
map<string, int> m;

for (const auto [key, value] : m) {
    cout << "{" << key << "," << value << "}\n";
}

void incr(map<string, int>& m)
{
    for (auto& [key, value] : m) {
        ++value;
    }
}
```

When structured binding is used for a class with no private data, it is easy to see how the binding is done: there must be the same number of names defined for the binding as there are nonstatic data members of the class, and each name introduced in the binding names the corresponding member.

It is also possible to handle classes where access is through member functions.

```c++
complex<double> z = {1, 2};
auto [re, im] = z + 2;
```


# Classes

The central language feature of C++ is the class. A class in a user-defined type provided to represent a concept in the code of a program.

## Types

Three important kinds of classes:

- concrete classes

- abstract classes

- classes in class hierarchies

An astounding number of useful classes turn out to be of one of these kinds.

### Concrete Types

The basic idea of _concrete classes_ is that they behave just like built-in types. The defining characteristic of a concrete type is that its representation is part of its definition. This allows implementations to be optimally efficient in time and space. It allows us to place objects of concrete types on the stack, in statistically allocated memory and in other objects. To increase flexibility, a concrete type can keep major parts of its representation on the free store and access them through the part stored in the class object itself.

The constructor/destructor combination is the basis of many elegant techniques. It is the basis for most C++ general resource management techniques. The _handle-to-data model_  is very commonly used to manage data that can vary in size during the lifetime of an object.


### Abstract Types

 An _abstract type_ is a type that completely insulates a user form implementation details. Since we don't know anything about the representation of an abstract type, not even its size, we must allocate objects on the free store and access them through references or pointers.
 
 ```c++
 class Container {
public:
    virtual double& operator[](int) = 0;
    virtual int size() const = 0;
    virtual ~Container() {}
};

Container c; // error
Container* p = new Vector_container(10); // container is an interface.
 ```

A function may take an object of `container` class.

```c++
void use(Container& c)
{
     const int sz = c.size();

     for (int i=0; i!=sz; ++i)
           cout << c[i] << '\n';
}
```

The class `container` provides the interface to a variety of other classes, called a _polymorphic type_.

Two implementations of `container` can be

```c++
class Vector_container : public Container {   // Vector_container implements Container
public:
     Vector_container(int s) : v(s) { }   // Vector of s elements
     ~Vector_container() {}

     double& operator[](int i) override { return v[i]; }
     int size() const override { return v.size(); }
private:
     Vector v;
};

class List_container : public Container {     // List_container implements Container
public:
     List_container() { }      // empty List
     List_container(initializer_list<double> il) : ld{il} { }
     ~List_container() {}
     double& operator[](int i) override;
     int size() const override { return ld.size(); }
private:
     std::list<double> ld;     // (standard-library) list of doubles
};

double& List_container::operator[](int i)
{
     for (auto& x : ld) {
           if (i==0)
                 return x;
           −−i;
     }
     throw out_of_range{"List container"};
}
```


Note the keyword `override`. It is optional, but using it allows the compiler to catch mistakes, such as misspelling of function names or slight differences between the type of a `virtual` function and its intended overrider. The explicit use of override is particularly useful in larger class hiearchies where it can otherwise be hard to know what is supposed to override what.

### Class Hierarchy

A class hierarchy offers two kinds of benefits: 

- Interface inheritance: The base class acts as an interface for the derived class. Such classes are often abstract classes.

- Implmentation inheritance: a base class provides functions or data that simplifies the implementation of derived class.

Classes in class hierarchies tend to be accessed through pointers or references and allocated on the free store.

```c++
enum class Kind { circle, triangle, smiley };

Shape* read_shape(istream& is)   // read shape descriptions from input stream is
{
     // ... read shape header from is and find its Kind k ...

     switch (k) {
     case Kind::circle:
          // read circle data {Point,int} into p and r
          return new Circle{p,r};
     case Kind::triangle:
          // read triangle data {Point,Point,Point} into p1, p2, and p3
          return new Triangle{p1,p2,p3};
     case Kind::smiley:
          // read smiley data {Point,int,Shape,Shape,Shape} into p, r, e1, e2, and m
          return ps;
     }
}

void user()
{
     std::vector<Shape*>v;
     while (cin)
          v.push_back(read_shape(cin));
     draw_all(v);                // call draw() for each element
     rotate_all(v,45);           // call rotate(45) for each element
     for (auto p : v)            // remember to delete elements
           delete p;
}
```

The manipulator of objects of `shape` has no idea of which kind of shapes it manipulates. It is crucial that all these subclasses override the virtual destructor so the manipulator may call it for every subclass. Otherwise, it does not know how to find the real destructor of an object without full type information of the objects.

Occasionally type information is lost and must be recovered. This typically happens when we pass an object to some system that accepts an interface specified by a base class.

```c++
Shape* ps {read_shape(cin)};

if (Smiley* p = dynamic_cast<Smiley*>(ps)) { // ... does ps point to a Smiley? ...
     // ... a Smiley; use it
}
else {
     // ... not a Smiley, try something else ...
}
```

Pointers may fail to be deleted and then resource leak happens.

```c++
class Smiley : public Circle {  // use the circle as the base for a face
public:
     Smiley(Point p, int rad) : Circle{p,r}, mouth{nullptr} { }

     ~Smiley()
     {
          delete mouth;
          for (auto p : eyes)
                delete p;
     }

     void move(Point to) override;

     void draw() const override;
     void rotate(int) override;

     void add_eye(Shape* s)
     {
          eyes.push_back(s);
     }
     void set_mouth(Shape* s);
     virtual void wink(int i);     // wink eye number i

     // ...

private:
     vector<Shape*> eyes;          // usually two eyes
     Shape* mouth;
};
```

One simple solution is to use `std::unique_ptr`. 

```C++
class Smiley : public Circle {
     // ...
private:
     vector<unique_ptr<Shape>> eyes; // usually two eyes
     unique_ptr<Shape> mouth;
};
```

This way, we no longer need a desctructor for `Smiley` since the destructor of `unique_ptr` takes responsiblity. The code using `unique_ptr` will be as efficient as code using raw pointers.

## Essential operations

Some operations such as initialization, assignment, cioy and move, are **fundamental** in the sense that language rules make assumptions about them.

Constructors, destructors, and copy and move operations for a type are not logically separate. We must define them as amtched set or suffer logical or performance problems.

```c++
class X {
public:
     X(Sometype);            // "ordinary constructor": create an object
     X();                    // default constructor
     X(const X&);            // copy constructor
     X(X&&);                 // move constructor
     X& operator=(const X&); // copy assignment: clean up target and copy
     X& operator=(X&&);      // move assignment: clean up target and move
     ~X();                   // destructor: clean up
     // ...
};
```

There are five situations in which an object can be copied or moved:

- as the source of an assigment 

- as an object initializer

- as a function argument

- as a function return value

- as an exception

An assignment uses a copy or move assignment operator. In principle, the other cases use a copy or move constructor. However, a copy or move constructor invocation is often optimized away by constructing the object used to initialize right in the target object.

Default implementation of special member functions can be explicitly specified, that is, generated by the compiler if needed:

```C++
class Y {
public:
     Y(Sometype);
     Y(const Y&) = default;   // I really do want the default copy constructor
     Y(Y&&) = default;        // and the default move constructor
     // ...
};
```

If an operation is not needed, `= delete` can be used.

Rule of zero: either define all of the essential operations or none (using the default for all).

### Conversions

A constructor taking a single argument defines a conversion from its argument type. 

```c++
complex z1 = 3.14; // from double
```

Sometimes, this implicit conversion causes problems. To avoid the situation, prepend `explicit` to a constuctor to make it a real "function".

```c++
classs Vector {
public:
    explicit Vector(int s);
}
```

### Member Initializer

```c++
class complex {
     double re = 0;
     double im = 0; // representation: two doubles with default value 0.0
public:
     complex(double r, double i) :re{r}, im{i} {}    // construct complex from two scalars: {r,i}
     complex(double r) :re{r} {}                     // construct complex from one scalar: {r,0}
     complex() {}                                    // default complex: {0,0}
     // ...
}
```

### Copy and Move

The default meaning of copy is memberwise copy: copy each member. However, when the class is responsible for an object through a pointer, teh default memberwise copy is typically a disaster.

Copying can be costly for large containers. `std::move` is a kind of cast, returning a rvalue reference to its argument.

```c++
Vector f()
{
     Vector x(1000);
     Vector y(2000);
     Vector z(3000);
     z = x;              // we get a copy (x might be used later in f())
     y = std::move(x);   // we get a move (move assignment is done here)
     // ... better not use x here ...
     return z;           // we get a move
}
```

The compiler is obliged (by the C++ standard) to eliminate most copies associated with initialization, so move constructors are not invoked as often as you might imagine. This copy elision eliminates even the very minor overhead of a move. On the other hand, it is typically not possible to implicitly eliminate copy or move operations from assignments, so move assignments can be critical for performance.
