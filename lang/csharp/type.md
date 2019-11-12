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

## Enums

An enum can be used as a more readable replacement for Boolean valeus as well: `DeviceState.On` versus `true`.


```csharp
enum ConnectionState : short // default to `int` which is recommended
{
    Disconnected,
    Connecting,
    Connected,
    Disconnecting
}
```

The base class for enums is `System.Enum`, which in turn is derived from `System.ValueType`. Enum type allows for assginment of unknown value other the named value at runtime through explicitly cast. However, there is an implicit conversion from `0` to any enum.

Besides inserting an enum value at the end of the list, one way to avoid changing enum values is to assign values explicitly.

C# does not support a direct cast between arrays of two different enums. The workaround is to cast first to `System.Array`. However, this approach is not guaranteed.

One of the conveniences associated with enums is that the `ToString()` method writes out the enum value identifier.

```csharp
enum Result
{
    OK,
    Error
}

      
        Result ok = (Result) System.Enum.Parse(typeof(Result), "OK");
        System.Console.WriteLine($"Test Enums {ok}");

        if (System.Enum.TryParse("Error", out Result res))
        {
                System.Console.WriteLine($"It's {res}");
        }
```

It's possible to define flag enum:

```csharp
[Flags] public enum FileAttributes  // FlagsAttribute
{
  ReadOnly =          1<<0,      // 000000000000000001
  Hidden =            1<<1,      // 000000000000000010
  System =            1<<2,      // 000000000000000100
  Directory =         1<<4,      // 000000000000010000
  Archive =           1<<5,      // 000000000000100000
  Device =            1<<6,      // 000000000001000000
  Normal =            1<<7,      // 000000000010000000
  Temporary =         1<<8,      // 000000000100000000
  SparseFile =        1<<9,      // 000000001000000000
  ReparsePoint =      1<<10,     // 000000010000000000
  Compressed =        1<<11,     // 000000100000000000
  Offline =           1<<12,     // 000001000000000000
  NotContentIndexed = 1<<13,     // 000010000000000000
  Encrypted =         1<<14,     // 000100000000000000
  IntegrityStream =   1<<15,     // 001000000000000000
  NoScrubData  =         1<<17,     // 100000000000000000
}
```

```csharp
[Flags] enum DistributedChannel
{
  None = 0,
  Transacted = 1,
  Queued = 2,
  Encrypted = 4,
  Persisted = 16,
  FaultTolerant =
      Transacted | Queued | Persisted
}
```

Flag check can be done using `EnumTypeName.HasFlag()` instead of using bit operations.

# Well-Formed Types

## Overriding Object Members

- overriding `ToString()`: Consider overloading the ToString() method whenever relevant diagnostic information can be provided from the output.

- overriding `GetHashCode()`: override `GetHashCode()` when overriding `Equals()`. Equal objects must have equal hash codes; `GetHashCode()`’s returns over the life of a particular object should be constant (the same value), even if the object’s data changes. In many cases, you should cache the method return to enforce this constraint.

- overriding `Equals()`: check for `null`; check for reference equality if the type is a reference type; check for equivalent types; TODO

## Operator Overloading

implemented as static methods.

`=` cannot be overloaded.

Unless the intent is for a type to act like a primitive type, operator overloading should be avoided.

- `==`/`!=`: reference equality check by default, which is a flaw. For the most part, the implementation for these operators can delegate the logic to `Equals`

It is possible to overload conversion operator.

TODO
