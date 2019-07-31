# Classes

In C++ `struct` find little use, they are mainly used to aggregate data within the context of classes or t odefine elaborate return values. C++ extends the C `struct` and `union` concepts by allowing the definition of member functions within these data types.

Use `const` reference to return a private member to prevent backdoor, that is, modifying objects without going through a mutator.

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

C++ supports data member initialization, which allows us to assign initial values to data members. The initial values do not have to be constant expressions. All constructors will apply the data member initialization s unless explicitly initialized otherwise.

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
        Stat(std::string const &fileName, std::sting const &searchPath) : d_hasPath(true) {//}
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
