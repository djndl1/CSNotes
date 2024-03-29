

# Forward/move helpers

All functions here are signal-safe.

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

When in a perfect forwarding scenario, 

```cpp
template <typename T>
void f(T&& t)
{
    g(std::forward<T>(t));
}
```

if `t` is a lvalue `int`, `T` is deduced to `int&`, then `std::forward<int&>(t)` returns `static_cast<int&>(t)`. If `t` is a rvalue `int`, `T` is deduced to `int`, then `std::forward<int>(t)` returns `static_cast<int&&>(t)`.

```cpp
template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
```

Given

- a rvalue `5`, `_Tp` is deduced to `int`, returns `static_cast<int&&>(__t)`;

- a lvalue `int b = 5`, `_Tp` is deduced to `int&`, reference-removed to `int`, returns `static_cast<int&&>(__t)`.

`std::move_if_noexcept` is used to move with strong exception-free guarantee.

```cpp
template<typename _B1, typename _B2>
    struct __and_<_B1, _B2>
    : public conditional<_B1::value, _B2, _B1>::type
    { };    

 template<typename _Tp>
    struct __move_if_noexcept_cond
    : public __and_<__not_<is_nothrow_move_constructible<_Tp>>,
                    is_copy_constructible<_Tp>>::type { };
                    
template<typename _Tp>
    constexpr typename
    conditional<__move_if_noexcept_cond<_Tp>::value, const _Tp&, _Tp&&>::type
    move_if_noexcept(_Tp& __x) noexcept
    { return std::move(__x); } 
```

`move_if_noexcept` obtains an rvalue reference to its argument if its move constructor does not throw exceptions or if there is no copy constructor (move-only type), otherwise obtains an lvalue reference to its argument. Note the second argument of `__and_` is with `::type`

TODO
