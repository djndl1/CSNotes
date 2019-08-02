# Library Overview

The facilities provided by the standard library can be 

- _runtime language support_, e.g. for allocation and runtime type information;

- _the C standard library_ with very minor modifications to minimize violations of the type system;

- _Strings_ with support for international character sets, localization, and read-only views of substrings;

- support for _regex matching_;

- _I/O_ streams framework;

- a framework of _containers and algorithms_, conventionally called the STL;

- support for _numerical computation_;

- support for _concurrent programming_, include `thread`s and locks;

- _parallel_ versions of most STL algorithms and some numerical algorithms;

- utilities to support template metaprogramming (type traits), STL-style generic programming, general programming (`variant` and `optional`) and `clock`;

- support for efficient and safe management of general resources, plus an interface to optional garbage collectors;

- _smart pointers_ for resource management;

- _Special-purpose containers_

- suffixes for popular units



# Strings and Regular Expressions

TODO

# Input and Output

TODO

# Containers

A class with the main purpose of holding objects is commonly called a _container_. An STL container is resource handler.

The standard library provides some of the most general and useful container types to allow the programmer to select a container tha
t best serves the needs of an application:

- `vector<T>`: variable size vector

- `list<T>`: a doubly-linked list

- `forward_list<T>`: a singly-linked list

- `deque<T>`: a double ended queue

- `set<T>`: a set (a `map` wit just a key and no value)

- `unordered_set<T>`: a set using a hashed lookup`

- `multiset<T>`: a set in which a value can occur many times

- `unordered_multiset<T>`: a multiset using a hashed lookup

- `map<K,V>`: an associative array

- `unordered_map<K,V>`: a map using a hashed lookup

- `multimap<K,V>`: a map in which a key can occur many times

- `unordered_multimap<K,V>`: a multimap using 

In addition, container adpators `queue<T>`, `stack<T>` and `priority_queue<T>` are provided. The standard library also provides more specialized container-like types, such as a fixed-size array `array<T,N>` and `bitset<N>`.

The standard containers and their basic operations are designed to be similar from a notational point of view. Furthermore, the meanings of the operations are equivalent for the various containers. Basic operations apply to every kind of container for which they make sense and can be efficiently implemented:

- `.begin()`; `.end()`; `.size()`; `.cbegin()`; `.cend()`

- `.empty()`; `.capacity()`; `reserve()`; `.resize()`

- `[]`: no range checking; `at()`: throws `out_of_range` if out of range

- `push_back()`: add to the end; `emplace_back()`

- `insert()`; `erase()`

- `=`; `==`, `!=`; `>`, `>=`, `<`, `<=`;



## `vector`

The most useful standard library container is `vector`, a sequence of elements of a given type. The elements are stored contiguously in memory, suitable as a default container.

A typical implementation of `vector` consists of a handle holding pointers to the first element, one-past-the-last element and extra allocated space. It holds an allocator, from which the `vector` acquire memory for its elements, defaulted to `new` and `delete`.

The standard-library `vector` implemented so that growing a `vector` by repeated `push_back()`s is efficient.

For classes that relies on `virtual` functions to get polymorphic behavior, store a pointer (including smart pointers)instead of objects itself in a container.

Many performance-critical applications use `vector`s and checking all subscripting implies a cost on the order of 10%. The standard library `vector` does not guarantee range checking. The `at()` operation is a vector subscript operation that throws an exception of type `out_of_range` if its argument is out fo the `vector`'s range. A range-`for` avoids range errors at no cost through iterators in the range.

## `list`

`list` is a doubly-linked list. We use `list` for sequences where we want to insert and delete elements without moving other elements. 

When we use a linked list, we tend not to access elements using subscripting the way for vectors. Instead, we search the list. Unless there is a compelling reason, use `vector`s, which perform better for traversal and for sorting and searching.

`forward_list` is a singly-linked list, which allows for only forward iteration, has no element count.

## `map`

The standard library offers a balanced binary search tree (usually a red-black tree, it thus has an order) called `map` . It is also known as an associative array or a dictionary. It is a container of pairs of values optimized for lookup. 

The `[]` operator returns the value of the input key, but also returns a default value for an invalid key.

## `unordered_map`

The standard library hashed containers are referred to as "unordered" because they don't require an ordering function. Given a good hash function, an `unordered_map` is much faster than a `map` for large containers. However, the worst-case behavior of an `unordered_map` with a poor hash function is far worse than that of a `map`.


The standard library provides a default hash function for built-ins and standard-library types.We can avoid explicitly passing the hash operation by defining it as a spcialization of the standard-library `hash`

```c++
namespace std { // make a hash function for Record

    template<> struct hash<Record> {
        using argument_type = Record;
        using result_type = std::size_t;

        size_t operator()(const Record& r) const
        {
             return hash<string>()(r.name) ^ hash<int>()(r.product_code);
        }
    };
}
```

# Algorithms

A data structure is not very useful on its own. We need operations for basic access such as adding and removing elements. The standard library provides the most common algorithms for containers in addition to providing the most common container types.

In the context of the C++ standard library, an algorithm is a function template operating on sequences of elements.


## Iterators

Iterators are used to separate algorithms and containers. An algorithm operates on its data through iterators and knows nothing about the container in which the elements are stored. Conversely, a container knows nothing about the algorithms operating on its elements; all it does is to supply iterators upon request (e.g., begin() and end()). This model of separation between data storage and algorithm delivers very general and flexible software.


For a container, a few iterators referring to useful elements can be obtained.

```cpp
template<typename C, typename V>
vector<typename C::iterator> find_all(C& c, V v)
{
     vector<typename C::iterator> res;          // typename, otherwise, ::iterator may be some constant
     for (auto p = s.begin(); p!=s.end(); ++p)
           if (*p==c)
                 res.push_back(p);
     return res;
}

void test()
{
     string m {"Mary had a little lamb"};
     for (auto p : find_all(m,'a'))
           if (*p!='a')
                 cerr << "a bug!\n";
}
```

A `vector` iterator might be a pointer or a pointer to the `vector` plus an index. A `list` iterator might be a pointer to a link node. What is common for all iterators is their semantics and the naming of their operations. In fact, any object that obeys a few simple rules like these is an iterator – Iterator is a concept.

The notion of iterators can be usefully applied to input and output.

```cpp
ostream_iterator<string> oo {cout};    // write strings to cout
*oo = "Hello, ";    // meaning cout<<"Hello, "
++oo;
*oo = "world!\n";   // meaning cout<<"world!\n"
```

Typically, `istream_iterators` and `ostream_iterators` are not used directly. Instead, they are provided as arguments to algorithms.

```c++
int main()
{
     string from, to;
     cin >> from >> to;             // get source and target file names

     ifstream is {from};            // input stream for file "from"
     ofstream os {to};              // output stream for file "to"

     set<string> b {istream_iterator<string>{is},istream_iterator<string>{}};     // read input
     copy(b.begin(),b.end(),ostream_iterator<string>{os,"\n"});                   // copy to output

     return !is.eof() || !os;           // return error state
}
```


## Predicates

```cpp
struct Greater_than {
     int val;
     Greater_than(int v) : val{v} { }
     bool operator()(const pair<string,int>& r) const { return r.second>val; }
};
```

```cpp
void f(map<string,int>& m)
{
     auto p = find_if(m.begin(),m.end(),Greater_than{42});
     // ...
}
```

Or we can use a lambda

```cpp
auto p = find_if(m.begin(), m.end(), [](const pair<string,int>& r) { return r.second>42; });
```

## (C++20) Concepts 

TODO

## Parallel Algorithms

TODO (not yet fully available)

# Utilities

## Resource Management

Besides RAII, the standard library provides two "smart pointers", `unique_ptr` and `shared_ptr`, to deal with objects allocated on the free store.

The most basic use of smart pointers is to prevent memory leaks caused by careless programming.

A `unique_ptr` is a handle to an individual object (or an array) in much the same way that a vector is a handle to a sequence of objects

```cpp
void f(int i, int j)    // X* vs. unique_ptr<X>
{
     X* p = new X;                // allocate a new X
     unique_ptr<X> sp {new X};    // allocate a new X and give its pointer to unique_ptr
     // ...

     if (i<99) throw Z{};         // may throw an exception
     if (j<77) return;            // may return "early"
     // ... use p and sp ..
     delete p;                    // destroy *p
}
```

A `unique_ptr` doesn't directly take away the ownership of a raw pointer

```cpp
int main(int argc, char *argv[])
{
  string* p = new string("Can you see me?");

  cout << "raw pointer has this: " << *p << "\n";

  unique_ptr<string> sp{p};

  cout << "After there is a unique_ptr: p = " << p << " *p =  " << *p << endl;

  cout << "unique_ptr has: sp=" << sp.get() << " *sp = " << *sp << endl;

  unique_ptr<string> sp2{move(sp)}; // no way to copy-construct a unique_ptr from a unique_ptr because of unique ownership

  cout << "Let's see if the ownership is transferred: " << " sp= " << sp.get() << " sp2=" << sp2.get() << " p=" << p << endl;
  return 0;
}
```

```bash
raw pointer has this: Can you see me?
After there is a unique_ptr: p = 0xb9ae70 *p =  Can you see me?
unique_ptr has: sp=0xb9ae70 *sp = Can you see me?
Let's see if the ownership is transferred:  sp= 0 sp2=0xb9ae70 p=0xb9ae70
```


Here, `sp` ensures the object is released. `unique_ptr` is a very lightweight mechanism with no space or time overhead compared to correct use of a built-in pointer. Its further uses include passing free-store allocated objects in and out of functions:

```cpp
unique_ptr<X> make_X(int i)
     // make an X and immediately give it to a unique_ptr
{
     // ... check i, etc. ...
     return unique_ptr<X>{new X{i}};
}
```

The `shared_ptr` for an object share ownership of an object, which will be destroyed only when the last of its `shared_ptr`s is destroyed. `shared_ptr` provides a form of garbage collection that respects the destructor-based resource management of the memory-managed objects. This is neither cost free nor exorbitantly expensive, but it does make the lifetime of the shared object hard to predict. Use `shared_ptr` only if you actually need shared ownership.

The standard library provides functions for constructing an object and returning an approoriate smart pointer, `make_shared()` and `make_unique()`. Using make_shared() is not just more convenient than separately making an object using new and then passing it to a shared_ptr, it is also notably more efficient because it does not need a separate allocation for the use count that is essential in the implementation of a shared_ptr.

Still, pointer semantics are not always recommended. We use pointer when

- we share an object, we need pointers (or references) to refer to the shared object;

- we refer to a polymorphic object in classical object-oriented code

- A shared polymorphic object typically requires `shared_ptr`s.

## `move()` and `forward()`

`std::move()` doesn't do anything more than casting its argument to an rvalue reference.

```cpp
template<typename _Tp>
constexpr typename std::remove_reference<_Tp>::type&&
move(_Tp&& __t) noexcept
{ 
    return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); 
}
```

It can be used to implement `std::swap`:

```cpp
#define _GLIBCXX_MOVE(__val) std::move(__val)

template<typename _Tp>
     inline
     typename enable_if<__and_<__not_<__is_tuple_like<_Tp>>,
                               is_move_constructible<_Tp>,
                               is_move_assignable<_Tp>>::value>::type
     swap(_Tp& __a, _Tp& __b)
     noexcept(__and_<is_nothrow_move_constructible<_Tp>,
                     is_nothrow_move_assignable<_Tp>>::value)
     {
       __glibcxx_function_requires(_SGIAssignableConcept<_Tp>)
 
       _Tp __tmp = _GLIBCXX_MOVE(__a);
       __a = _GLIBCXX_MOVE(__b);
       __b = _GLIBCXX_MOVE(__tmp);
     }
```

The state of a moved-from object is in general unspecified, but all standard-library types leave a moved-from object in a state where it can be destroyed and assigned to. It would be unwise not to follow that lead.

`std::forward()` differs from the simpler `std::move()` by correctly handling subtleties to do with lvalue and rvalue.

```cpp
template<typename _Tp>
constexpr _Tp&&
forward(typename std::remove_reference<_Tp>::type& __t) noexcept
{ return static_cast<_Tp&&>(__t); }
    
template<typename _Tp>
constexpr _Tp&&
forward(typename std::remove_reference<_Tp>::type&& __t) noexcept
{
    static_assert(!std::is_lvalue_reference<_Tp>::value, "template argument"
    " substituting _Tp is an lvalue reference type");
    return static_cast<_Tp&&>(__t);
}
```


## (C++20) `std::span` 

TODO

## Specialized Containers

- built-in containers

- `array<T,N>`: a fixed-size continuously allocated sequence of N elements of type T. 

An `array` can be allocated with its elements on the stack, in an object, or in static storage. The elements are allocated in the scope where the `array` is defined. An array is best understood as a built-in array with its size firmly attached, without implicit, potentially surprising conversions to pointer types, and with a few convenience functions provided. There is no overhead (time or space) involved in using an array compared to using a built-in array. An array does not follow the “handle to elements” model of STL containers. 

An `array` can be explicitly passed a C-style function that expects a pointer.

Occasionally, there is a significant performance advantage to be had by directly accessing elements allocated on the stack rather than allocating elements on the free store. On the other hand, the stack is a limited resource (especially on some embedded systems), and stack overflow is nasty. 

An `array` knows its size, so it is easy to use with standard-library algorithms, and it can be copied using `=`. And it saves the programmer from implicit conversion to pointers like the old built-in array and thus avoid disasters of wrong offsets caused by wrong type information.

- `bitset<N>`: a fixed-size sequence of N bits. For sets of bits that don't fit into a `long long int`, using a `bitset` is much more convenient than using integers directly.

```cpp
bitset<9> bs1 {"110001111"};
bitset<9> bs2 {0b1'1000'1111};
```

- `pair<T,U>`: two elements of types T and U

- `tuple<T...>`: a sequence of an arbitrary number of elements of arbitrary heteoregenous types, contiguously allocated. Accessing members of a tuple by their index is general, ugly, and somewhat error-prone. Fortunately, an element of a tuple with a unique type in that tuple can be “named” by its type.

- `basic_string<C>`: a sequence of characters of type `C`

- `valarray<T>`: an array of numeric values of type `T`; provides numeric operations.



## (C++17) Alternatives

TODO

## Allocators

By default, standard-library containers allocate space using `new`. Operators `new` and `delete` provide a general free store (also called dynamic memory or heap) that can hold objects of arbitrary size and user-controlled lifetime.

The standard-library containers offer the opportunity to install allocators with specific semantics where needed. This has been used to address a wide variety of concerns related to performance (e.g., pool allocators), security (allocators that clean-up memory as part of deletion), per-thread allocation, and non-uniform memory architectures (allocating in specific memories with pointer types to match).

TODO

## Time

`<chrono>` provides facilities for dealing with time.

```cpp
auto t0 = high_resolution_clock::now();
do_work();
auto t1 = high_resolution_clock::now();
cout << duration_cast<milliseconds>(t1−t0).count() << "msec\n";
```

## Function Adaption

Use lambda expressions

```cpp
void draw_all(vector<Shape*>& v)
{
     for_each(v.begin(),v.end(),[](Shape* p) { p−>draw(); });
}
```

Use `std::mem_fn`, which produces a function object that can be called as a nonmember function. `std::mem_fn` generates wrapper objects for pointers to members, which can store, copy, and invoke a pointer to member. Both references and pointers (including smart pointers) to an object can be used when invoking a `std::mem_fn`.

```cpp
void draw_all(vector<Shape*>& v)
{
     for_each(v.begin(),v.end(),mem_fn(&Shape::draw));
}
```

Use `std::function`, a type that can hold any object that can be invoked using `()`. `function`s are useful for callbacks, for passing operations as arguments, for passing function objects, etc. it may introduce some run-time overhead compared to direct calls, and a function, being an object, does not participate in overloading. If you need to overload function objects (including lambdas), consider `overloaded`.

```cpp
int f1(double);
function<int(double)> fct1 {f1};                // initialize to f1

int f2(string);
function fct2 {f2};                             // fct2's type is function<int(string)>

function fct3 = [](Shape* p) { p−>draw(); };    // fct3's type is function<void(Shape*)>
```

## Type functions

A type function is a function that is evaluated at compile time given a type as its argument or returning a type. The standard library provides a variety of type functions to help library implementers (and programmers in general) to write code that takes advantage of aspects of the language, the standard library, and code in general.

Such type functions are part of C++’s mechanisms for compile-time computation that allow tighter type checking and better performance than would otherwise have been possible. Use of such features is often called metaprogramming or (when templates are involved) template metaprogramming.

TODO


In `<type_traits>`, the standard library offers simple type functions, called _type predicates_ that answers a fundamental question about types. They are most useful when we write templates.

Obvious ways of using type predicates includes conditions for `static_assert`s, compile-time `if`s, and `enable_if`s. The standard-library `enable_if` is a widely used mechanism for conditonally introducing definitions. The syntax of enable_if is odd, awkward to use, and will in many cases be rendered redundant by concepts. It relies on a subtle language feature called SFINAE (“Substitution Failure Is Not An Error”).

# Numerics

TODO


# Concurrency

Concurrency is widely used to improve throughput or to improve responsiveness. The standard library supoort for concurrency is primarily aimed at supporting system-level concurrency rather than directly providing sophisticated higher-level concurrency models. The features are built directly upon what operating systems offer and do not incur performance penalties compared with those.

A computation that can potentially be executed concurrently with other computation is a _task_. A thread is the system-level representation of a task in a program.

```cpp
void f();                 // function

struct F {                // function object
     void operator()();   // F's call operator (§6.3.2)
};

void user()
{
     thread t1 {f};       // f() executes in separate thread
     thread t2 {F()};     // F()() executes in separate thread

     t1.join();           // wait for t1 to terminate
     t2.join();           // wait for t2 to terminate
}
```

Inter-thread communication is typically controlled by locks or other mechanisms to prevent data reaces (uncontrolled concurrent access to a variable).
