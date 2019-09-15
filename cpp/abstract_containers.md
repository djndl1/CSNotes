# `std::deque` in `<deque>`

A doubly ended data structure. It allows for reading and writing at both ends. A deque is a combination of a vector and two queues, operating at both ends of the vector. In situations where random insertions and the addition and/or removal of elements at one or both sides of the vector occurs frequently using a deque should be considered.

# `std::map` in `<map>`

A sorted associative container with unique keys, usually implemented as a rb-tree.

A map sorts its keys, the key's `operator<` must be defined.

```cpp
template <class _Key, class _Tp, class _Compare = less<_Key>,
          class _Allocator = allocator<pair<const _Key, _Tp> > >
class _LIBCPP_TEMPLATE_VIS map
{
public:
    // types:
    typedef _Key                                     key_type;
    typedef _Tp                                      mapped_type;
    typedef pair<const key_type, mapped_type>        value_type;
    typedef typename __identity<_Compare>::type      key_compare;
    typedef typename __identity<_Allocator>::type    allocator_type;
    typedef value_type&                              reference;
    typedef const value_type&                        const_reference;
...
```

The map supports, in addition to the standard operators for containers, the index operator. The index operator may be used to retrieve or reassign individual elements of the map.

```cpp
// an iterator to the element having the given key
map::iterator find(key);

// insers a new `value_type` into the map.
pair<map::iterator, bool> insert(value_type);

// returns a copy of the objeect used by the map to compare keys
key_compare key_comp()

// the first element that is not less than key
map::iterator lower_bound(key);
// the first element whose key is greater than the specified key
map::iterator upper_bound(key);
// [lower_bound, upper_bound]
pair<iterator, iterator> equal_range(key);
```

# `std::multimap` in `<map>`

A sorted associative container with multiple identical keys. It does not support the index operator.

# `std::set` in `<set>`

A sorted collection of values. (Apparently implemented as a red-black tree).


# `std::multiset` in `<set>`

A sorted collection of values with multiple identical entries.

# `std::stack` in `<stack>`

FILO, LIFO. Implemented using a deque. Only the basic set of container operators are supported by the stack.

# `std::unordered map`, `std::unordered_multimap` in `<unordered_map>`

Hash is a far faster way to store and retrieve data. The efficiency of a `unordered_map` in terms of speed should greatly exceed the efficiency of the `map`.

```cpp
hasher key_eq() const; // a copy of the `key_equal` function

float load_factor() const; // the container's current load factor

size_t max_bucket_count(); // returns the maximum number of buckets this `unordered_map` may contain

size_t bucket(key_type,const &key); //  the index location where key is stored

size_t bucket_count(); // the number of slots used by the containers.

size_t bucket_size(size_t index); // the number of objects stored at a bucket

hasher hash_function() const; // a copy of the hash function used by the container

void rehash(size_t size); // 
```

The `unordered_multimap` allows multiple objects using the same keys to be stored in an unordered
map. It does not support `operator[]` and `at()`.

# `std::unordered_set`, `std::unordered_multiset` in `<unordered_set>`

Elements stored in the `unordered_set` are immutable but they can be inserted or removed. The unordered_multiset allows multiple objects using the same keys to be stored in an unordered set. 

# (C++14) Heteregenenous Lookup

Since the C++14 standard arbitrary lookup key types can be used provided a comparison operator is available to compare that type with the container’s key type. Thus, a `char const∗ key` (or any other type for which an `operator<` overload for `std::string` is available) can be used to lookup values in a `map<std::string, ValueType>`. This is called _heterogeneous lookup_.

# `std::complex` in `<complex>`

Several mathematical functions are available for the complex container, such as `abs`, `arg`, `conj`, `cos`, `cosh`, `exp`, `log`, `norm`, `polar`, `pow`, `sin`, `sinh` and `sqrt`. All these functions are free functions, not member functions, accepting complex numbers as their arguments. Complex numbers may be extracted from `istream` objects and inserted into `ostream` objects.
