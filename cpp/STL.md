The Standard Template Library is a general purpose library consisting of containers, generic algorithms, iterators, function objects, allocators, adaptors and abstract data structures. 

# Predefined Function Objects `<functional>`

Function objects play important roles in generic algorithms.

```cpp
class CaseInsensitiveComp {
    public:
        bool operator()(string const &left, string const &right) const 
        {
            return strcasecmp(left.c_str(), right.c_str()) < 0;
        }
}

sort(argv, argv + argc, CaseInsensitiveComp());
```

The arithmetic function objects support the standard arithmetic operations: 

- addition: `plus<T>`, calling the binary `operator+`;

- subtraction: `minus<T>`, calling the binary `operator-`;

- multiplication: `multipliers<T>`, calling the binary `operator*`;

- division: `divides<T>`, calling `operator/`;

- modulo: `modulus<T>`, calling `operator%`;

- negation: `negate<T>`, calling unary `operator-`;

The STL supports the following set of relational function objects:

- `equal_to<T>`: calls `operator==`;

- `not_equal_to<T>`: calls `operator!=`;

- `greater<T>`: calls `operator>`;

- `greater_equal`: calls `operator>=`;

- `less<T>`: calls `operator<`;

- `less_equal<T>`: calls `opeator<=`;

Logical function object:

- `logical_and<T>`: `operator&&`

- `logical_or<T>`: `operator||`

- `logical_not<>`: `operator!`

```cpp
std::transform(bArr, bArr + bArrSize, logical_not<bool>()); # inverts a boolean array
```

(C++17) A negator (`std::not_fn`) is a function object toggling the truth value of a function that's called from the negator: if the function returns `true`, the negator returns false and vice versa. This is useful when the negated function cannot be modified.

# Iterators

Iterators are objects acting like pointers (a generalization, plain pointers can be used as iterators). Iterators have the following general characteristics:

- two iterators may be compared for equality using `==` and `!=`. The ordering operators can usually not be used;

- Given an iterator `iter`, `*iter` represents the object the iterator points to, `iter->member` is legal.

- `++iter` and `iter++` advances the iterator to the next element.

- Pointer arithmetic may be used with iterators of containers storing their elements consecutively in memory.

- Merely defining an iterator is comparable to having a null pointer.

 In general, iterators must define `operator==`, `operator!=`, `operator++`, `operator*`.

Standard practice requires iterator ranges to left inclusive (`[left, right)`). The iterator range is empty when `left==right`.

```cpp
vector<string> args(argv, argv + argc);

    for_each(args.cbegin(), args.cend(), [](const string& s) -> void { cout << s << ' '; });
    cout <<  '\n';

    for_each(args.rbegin(), args.rend(), [](const string& s) -> void { cout << s << ' '; });
    cout <<  '\n';
```

```bash
djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./a.out a b c d e f g h i j k l m n o p q r s t
./a.out a b c d e f g h i j k l m n o p q r s t 
t s r q p o n m l k j i h g f e d c b a ./a.out 
```



The STL defines five types of iterators. Each category of iterator is defined by the operations that can be performed on it:

- `InputIterator`: The deference operator is guaranteed to work as rvalue in expressions. They are used to read from a container.

- `OutputIterator`: The deference operator is guaranteed to work as an lvalue in expression, but not necessarily as rvalue. They are used to write to a container.

- `Forwarditerator`: combine `InputIterator` and `OutputIterator`. They can be used to traverse containers in one direction, for reading and/or writing.

- `BidirectionalIterators`: They can traverse containers in both directions, for reading and writing.

- `RandomAccessIterators`: provides random acess to container elements.

`std::distance` expects two `InputIterator`s and returns the number of elements between them. `std::size` returns the number of elements in a container.

Generic algorithms often require a target container into which the results of the algorithm are deposited. Situations exist where pointer arithmetic cannot be used. Analogously, the number of resulting elements sometimes differs from the number of elements in the initial range. In situations like these an inserter adaptor function can often be used to create elements in the destination container.

- `back_inserter`: calls the container's `push_back` member to add new elements at the end of the container.

- `front_inserter` calls the container’s push_front member, adding new elements at the beginning of the container. 

-  `inserter` calls the container’s `insert` member adding new elements starting at a specified starting point.


The `istream_iterator<Type>` can be used to define a set of iterators for `istream` objects. 

```cpp
vector<string> vs;
copy(istream_iterator<string>(cin), istream_iterator<string>(), back_inserter(vs));
```

An `ostream_iterator<Type>` adaptor can be used to pass an `ostream` to algorithms expecting an `OutputIterator`.

```cpp
cin.unsetf(ios::skipws);
copy(istream_iterator<char>(cin), istream_iterator<char>(),
ostream_iterator<char>(cout));
```

Input iterators are also available for `streambuf` objects: `istreambuf_iterator`. Output iterators are also available for `streambuf` objects: `ostreambuf_iterator`.

```cpp
istreambuf_iterator<char> in(cin.rdbuf());
istreambuf_iterator<char> eof;
ostreambuf_iterator,char> out(cout.rdbuf());
copy(in, eof, out);
```
