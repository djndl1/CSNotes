# `std::deque` in `<deque>`

A doubly ended data structure. It allows for reading and writing at both ends. A deque is a combination of a vector and two queues, operating at both ends of the vector. In situations where random insertions and the addition and/or removal of elements at one or both sides of the vector occurs frequently using a deque should be considered.

# `std::map` in `<map>`

A sorted associative container with unique keys.

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
