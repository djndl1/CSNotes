# overloading `operator[]()`

```cpp
int &operator[](size_t index);
int operator[](size_t index) const; // const object &operator[](size_t) const, also, const member specifier is needed in case *this is const
```
# overloading the insertion and extraction operators

They can be defined as overloaded free functions.

```cpp
std::ostream& operator<<(std::ostream &out, Person const& person);
```


# Conversion Operators

`char const *` can be easily converted to `std::string` thanks to assignment operator overloading and cosntructor overloading. However, the other way around is not possible unless using a conversion operator, in the form of `operator <type>() const;`

```cpp
class String
{
        char *d_string;
        public:
        String();
        String(char const *arg);
        ~String();
        String(String const &other);
        String &operator=(String const &rvalue);
        String &operator=(char const *rvalue);
};


String::operator char const *() const {
    return d_string;
}
```

Conversion operators usually are const member functions: they are automatically called when their
objects are used as rvalues in expressions having a type lvalue. As a rule of thumb: classes should define at most one conversion operator. Multiple conversion operators may be defined but frequently result in ambiguous code. 

A conversion operator should return an rvalue. It should do so to enforce data-hiding and because it is the intended use of the conversion operator. Defining a conversion operator as an lvalue (e.g., defining an `operator int &()` conversion operator) opens up a back door, and the operator can only be used as lvalue when explicitly called.

# `explicit`

Conversions are not only performed by conversion operators, but also by constructors accepting one argument. It is good practice to prevent implicit promotions by using the `explicit` modifier when declaring a constructor. Constructors using the explicit modifier can only be used to construct objects explicitly.

C++ supports _explicit conversion operators_. When defining explicit conversion operators implicit conversions are prevented. Such conversion operators can only be used in situations where the converted type is explicitly required (as in the condition clauses of if or while statements), or is explicitly requested using a `static_cast`.


# Overloading the increment and decrement operators

When used as a postfix operator, the value's object is returned as an rvalue. Used as prefix, the variable is incremented and its value is returned as lvalue. When calling the increment or decrement operator using its full member function name then any int argument passed to the function results in calling the postfix operator. Omitting the argument results in calling the prefix operator. 

```cpp
T& operator++(); // prefix
T operator++(int); // postfix
```

```cpp
Unsigned &operator++()
{
    Unsigned tmp{*this};
    
    tmp.inc();
    swap(swap);
    
    return *this;
}

Unsigned operator++()
{
    Unsigned tmp{*this};
    
    tmp.inc();
    swap(tmp);
    
    
    return tmp;
}
```

# Overloading binary operators

https://stackoverflow.com/questions/4622330/operator-overloading-member-function-vs-non-member-function

Binary operators supporting promotions for either their left-hand side operand or right-hand operand should be declared as free operators/functions. If either of the parameters is the intended class type, the other is promoted wheenver possible. No promotions occur when neither is of the intended class type. An ambiguity occurs when promotions to different classes are possible for the two operands.

Compound assignment operators are always implemented as genuine member functions and they usually return references to the objects for which the binary compound assignment operators were requested.

```cpp
class Binary {
    public:
        Binary();
        Binary(int value);
        
        friend Binary operator(Binary const &lhs, Binary const &rhs);
        
        Binary &operator+=(Binary const &rhs)
        {
            Binary tmp{*this};
            tmp.add(rhs);
            swap(tmp);
            
            return *this;
        }
        
        }
};

Binary operator(Binary const &lhs, Binary const &rhs)
{
    Binary tmp{lhs};
    tmp.add(rhs);
    
    return tmp;
}
```

A temporary object bound to a reference parameter in a function call persists until the completion of the full-expression containing the call. The lifetime of a temporary bound to the returned value in a function return statement is not extended; the temporary is destroyed at the end of the full-expression in the return statement. A temporary object cannot itself be returned as the function's return value. Function implementing binary operators are factory functions.

```cpp
Binary operator+(Binary &&lhs, Binary const &rhs)
        {
            lhs.add(rhs);
            return std::move(lhs);
        }
        
Binary operator+(Binary &&lhs, Binary const &rhs)
        {
            Binary ret{std::move(lhs)};
            ret.add(rhs);
            return ret;
        }
```

## Member function reference bindings

We can inform the compiler that a particular member should only be used when the objects calling those members is an anonymous temporary object or a non-anonymous object.

Functions provided with rvalue reference bindings are selected by the compiler when used by anonymous temporary objects, whereas functions provided with lvalue reference bindings are selected by the compiler when used by other types of objects.

```cpp
Binary Binary::operator+=(Binary const &rhs) && // semantically questionable, += shouldn't return an rvalue

{
    add(rhs);
    return std::move(*this);
}

Binary &Binary::operator+=(Binary const &other) &
{
    Binary tmp{*this};
    tmp.add(other);
    swap(tmp);
    return *this;
}
```

# Overloading `operator new(size_t)`, `operator delete(void*)`

The overloaded versions may define multiple parameters.

```cpp
class String {
    std::string *d_data;
    
    public:
        void *operator new(size_t size) // the size is deduced from the size of objects of the class 
        {
            return memset(::operator new(size), 0, size);
        }
};
```

When `new String` is used, the `String::operator new` is called. This `operator new` is actually a static member function.

Placement `new` can also be overloaded.

```cpp
void *String::operator new(size_t size, char *memory)
{
    return memset(memory, 0, size);
}

char buffer[sizeof(String)];
String *sp = new(buffer) String;
```

It is good practice to overload `operator delete` whenever `operator new` is also overloaded. `operator delete` is called when deleting a dynamically allocated object after executing the destructor of the associated class. The overloaded `operator delete` may do whatever it wants to do with the memory pointed to.


# Overloading `new[]` and `delete[]`

```cpp
// placement new
void *String::operator new[](size_t size, char *memory)
{
    return memset(memory, 0, size);
}

char buffer[12 * sizeof(String)];
String *sp = new(buffer) String[12];
```
Although the addresses returned by `new` and `new[]` point to the allocated object(s), there is an additional `size_t` value available immediately before the address returned by `new` and `new[]`.  This size_t value is part of the allocated block and contains the actual size of the block.  `operator new[]` does not receive the address returned by `new[]` but rather the address of the initial `size_t` value.

`operator delete[]` may also be overloaded using an additional `size_t` parameter.

```cpp
void String::operator delete[](void *p, size_t size)
{
    cout << "deleting << size << " bytes\n" ;
    ::operator delete[](ptr);
}
```

`size` is automatically initialized to the size (in bytes) of the block of memory to which `void *p` points. If this form is defined, simple `operator delete[](void *)` should not be defined to avoid ambiguities.


```cpp
#include <cstddef>
#include <iostream>

using namespace std;

struct Demo
{
    size_t idx;
    Demo()
        {
            cout << "default cons\n";
        }
    ~Demo()
        {
            cout << "destructor\n";
        }
    void *operator new[](size_t size) { return ::operator new(size); }
    void operator delete[](void *vp, size_t sz)
                            {
                                cout << "delete[] for: " << vp <<  "of size " << sz << '\n';
                                ::operator delete[](vp);
                            }
};

int main()
{
    Demo *xp;
    cout << ((size_t *)(xp = new Demo[3]))[-1] << '\n';
    cout << xp << '\n';
    cout << "==================\n";
    delete[] xp;
}
```

```bash
default cons
default cons
default cons
3
0x55b827cb5e78
==================
destructor
destructor
destructor
delete[] for: 0x55b827cb5e70of size 32
```

`new[]` is exception safe even when only some of the objects were properly constructed.

https://isocpp.org/files/papers/n3778.html

https://stackoverflow.com/questions/34407032/how-would-i-use-the-sized-operators-delete-delete-and-why-are-they-better
