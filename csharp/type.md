# Value Type

- Do not create value types that consume more than 16 bytes of memory.

- `struct`: defines an aggregate of value type. `struct`s have a default constructor that initializes all fields to their default values. Failure to initialize all data within the struct causes a compile-time error. 

```csharp
struct Angle
{
  public Angle(int degrees, int minutes)
      : this( degrees, minutes, default(int) )
  {
  }
  // ...
}
```

Value types are meant to be immutable. In scenarios where modification is desirable, create a new instance. It's a value after all. Favor read-only, automatically implemented properties over fields within structs starting with C# 6.0.

All value types are implicitly sealed. Value types can implement interfaces.

## Boxing

Type conversion from a value type to an object of a certain class/interface.

Converting a variable of value type that directly refers to its data to a reference type that refers to a location on the garbage-collected heap involves several steps:

1. Memory is allocated on the heap;

2. The value of the value type is copied from its current storage location into the newly allocated location on the heap;

3. The result of the conversion is a reference to the new storage location on the heap.
