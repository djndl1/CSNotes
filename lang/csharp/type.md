# Value Type

Value types do not always reside on stack. Value type data inside a class are in the same region allocated for the class instance.

- Do not create value types that consume more than 16 bytes of memory.

- `struct`: defines an aggregate of value type. `struct`s have a default constructor that initializes all fields to their default values. Failure to initialize all data within the struct causes a compile-time error. C# disallows field initializers in a struct. The constructor initializes the property rather than the field produces a compile error. it is not legal to access `this` until the compiler knows that all fields have been initialized (why `this.property`is disallowed). Do favor read-only, automatically implemented properties over fields within structs.

```csharp
struct Angle
{
    public Angle(int degrees, int minutes, int seconds)
    {
        Degrees = degrees;
        Minutes = minutes;
        Seconds = seconds;
    }

    public int Degrees { get; }  // `readonly`
    public int Minutes { get; }
    public int Seconds { get; }

    public Angle Move(int degrees, int minutes, int seconds)
    {
        return new Angle(
                         Degrees + degrees,
                         Minutes + minutes,
                         Seconds + seconds
                         );
    }
}

class Coordinate
{
    public Angle Longitude { get; set; }
    public Angle Latitude { get; set; }
}
```


Value types are meant to be immutable. In scenarios where modification is desirable, create a new instance. It's a value after all. Favor read-only, automatically implemented properties over fields within structs starting with C# 6.0. All value types are implcitly sealed. All non-enum value types derive from `System.ValueType`. Value types can implement interfaces. The default implementation for `GetHashCode()` and `Equals()` have some different behavior that need to be handled.

All value types are implicitly sealed. Value types can implement interfaces.

Invoking `new` with a value type causes the runtiem to create a new instance on the temporary storage pool, initialize all of its fields to their default values and call the constructor by passing the temporary storage location as a `ref` variable as `this`, resulting in the value being stored in the temporary storage location. Structs are copied by value. They do not have referential identity as reference types do. No part of the runtime tracks how many copies of a given value type exist at any moment.

- `default`: `default(int)` and `new int()` produce the same value.

## Boxing

Boxing: Type conversion from a value type to an object of a certain class/interface.

Converting a variable of value type that directly refers to its data to a reference type that refers to a location on the garbage-collected heap involves several steps:

1. Memory is allocated on the heap;

2. The value of the value type is copied from its current storage location into the newly allocated location on the heap;

3. The result of the conversion is a reference to the new storage location on the heap.

Boxing can occur in some unexpected situations, and frequent occurrences can have a significant impact on performance. `ArrayList` maintains a list of references to objects, so adding an integer or floating point number to the list will box the value so that a reference can be obtained. Developers need to pay special attention and notice situations where it could potentially occur repeatedly and affect performance. Also, boxed `int` cannot be directly converted to an unboxed `double`.

How Boxing can be avoided TODO


