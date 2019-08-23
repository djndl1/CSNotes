The named requirements here are the named requirements used in the normative text of the C++ standard to define the expectations of the standard library.

# Basic

- `DefaultConstructible`: not just through default constructors, but mostly.

- `MoveConstructible`: 

```cpp
class movable {
private:
    int x;
public:
    movable() : x{0} {}
    movable(int x) : x{x} {}
    movable(movable&& other) : x{std::move(other.x)} {}
    movable& operator=(movable&& rhs)
        {
            this->x = rhs.x;
            rhs.x = 0;

            return *this;
        }

};

int main(int argc, char *argv[])
{
    movable a_mov{5};

    // movable b = a_move
    // error: use of deleted function 'constexpr movable::movable(const movable&)'
    movable b = std::move(a_mov);
    
    //movable c{b};
    //  error: use of deleted function 'constexpr movable::movable(const movable&)'

    movable c{std::move(b)};
}
```

A class does not have to implement a move constructor to satisfy this type requirement: a copy constructor that takes a `const T&` argument can bind rvalue expressions.

- `CopyConstructible`: requires `MoveConstructible` first.

- `MoveAssignable`: The type does not have to implement move assignment operator in order to satisfy this type requirement: a copy assignment operator that takes its parameter by value or as a `const Type&`, will bind to rvalue argument.

- `CopyAssignable`: requires `MoveAssignable` first.

- `Destructible`: all scalar types meet the requirement of Destructible, while array types and reference types do not. Note that std::is_destructible allows arrays and reference types.
