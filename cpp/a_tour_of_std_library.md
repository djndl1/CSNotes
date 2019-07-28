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
