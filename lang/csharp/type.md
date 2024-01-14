# Well-Formed Types

## Override `Equals(obj)`

The default implementation uses `ReferenceEquals` for reference types. 

For value types, two objects are equal if they are of the same type 
and if the values of the public and private fields of the two objects are equal (`Object.Equals()` is called on each field of the instance if any member is a reference). Always implement `.Equals()` for values types, otherwise the default reflection-based equality test offer poor performance.
Calling `ReferenceEquals()` on values type will always return `false` as they are in different boxes.

Note that equality test methods should not be confused with `IComparable.CompareTo()`, where equality only means the same position in the sort order.

Typical steps:

- check for `null`

- check for equivalent types, be careful when using `is` as it identifies derived types as equivalent types.

- invoke a typed `Equals()`

- possibly check for equivalent hash codes to short-circuit fields checking.

- check `base.Equals()` if implemented

- comparing identifying fields

- override `GetHashCode()`

- override the `==` and `!=` operators if needed

- Never throw any exception

Additionally, typed equality test `IEquatable<T>` may be implemented, especially for value types since they are sealed and this avoids boxing performance penalty.

## Overriding `GetHashCode()`

Overriding `GetHashCode()` of a type is a good practice to be used as a key into a hash table collection.

- (required) if `a.Equals(b)` then `a.GetHashCode() == b.GetHashCode()`, the converse is not true.

- (required) For mutable reference types, override `GetHashCode()` only if the identifying fields are immutable, or the hash code does not change while the object is contained in a hash collection. Otherwise, be careful not to modify object values while the object is stored in a hash table.

- (required) `GetHashCode()` should not throw any exception.

- (for performance) unique whenever possible

- (for performance) evenly distributed

- (for performance) small difference between objects results in large difference between hash codes

- (for performance) inexpensive to compute

- (for security) hard to craft an object of a particular hash code

By default, a reference type's hash code is computed based on its reference. For value types, they are computed based on member field values (Do override it for better performance).

For ease of implementation, use `System.HashCode` (.NET Standard 2.1 Only) to combine them.

# Indexers

Indexers allow instances of a class or struct to be indexed just like arrays. The compiler will generate an `Item` property (not directly accessible), and the appropriate accessor methods. Indexers are most frequently implemented in types whose primary purpose is to encapsulate an internal collection or array.

```csharp
class SampleCollection<T>
{
   // Declare an array to store the data elements.
   private T[] arr = new T[100];

   // Define the indexer to allow client code to use [] notation.
   public T this[int i]
   {
      get { return arr[i]; }
      set { arr[i] = value; }
   }
}
```

C# doesn't limit the indexer parameter type to integer.

## Operator Overloading

implemented as static methods, except for `=`, `x.y`, `f(x)`, `new`, `typeof`, `default`, `checked`, `unchecked`, `delegate`, `is`, `as`, `=>`, `=`, `&&`, `||` every other operator may be overloaded. Unless the intent is for a type to act like a primitive type, operator overloading should be avoided. 

It is possible to overload equality/inequality operators, comparision operators, binary operators `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<<`, `>>` (which overload corresponding assignment operators automatically), binary operators `+`, `-`, `!`, `~`, `++`, `--`, `true`, `false` (`true` and `false` allow types to be used in `if`, `while`, and `for`), conversion operator.

Although `&&` and `||` are not overridable, [User-Defined Conditional Logical operators](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#12143-user-defined-conditional-logical-operators) overriding `&`, `|`, `true`, `false` effectively provides overloading.

If `==` is overridden, then `Equals()` should be overridden to provide the same behavior. The converse is not true unless the is type intended as value types.

## Garbage Collection

The garbage collector is responsible only for restoring memory; it does not handle other resources such as database connections, handles (files, windows, etc.), network ports, and hardware devices such as serial ports.

Weak references `System.WeakReference`: do not prevent garbage collection. Designed for reference objects that are expensive to create yet too expensive to keep around.


```csharp
public static class ByteArrayDataSource
{
  static private byte[] LoadData()
  {
      // Imagine a much lager number
      byte[] data = new byte[1000];
      // Load data
      // ...
      return data;
  }

  static private WeakReference<byte[]>? Data { get; set; }

  static public byte[] GetData()
  {
      byte[]? target;
      if (Data is null) // first time
      {
          target = LoadData(); // a strong ref before assigninng a weak ref
          Data = new WeakReference<byte[]>(target);
          return target;
      }
      else if (Data.TryGetTarget(out target)) // try to retrieve the loaded data by checking the weak reference
      {
          return target;
      }
      else
      {
          // Reload the data and assign it (creating a strong
          // reference) before setting WeakReference’s Target
          // and returning it.
          target = LoadData();
          Data.SetTarget(target);
          return target;
      }
  }
}
```

The finalizer has a syntax similar to the C++ destructor. Finalizers are not responsible for memory deallocation.
Limiting the need for a finalizer to only objects that need resource cleanup that the runtime isn't already aware of (resources that don't have finalizers) is an important guideline that significantly reduces the scenarios where it is necessary to implement a finalizer. Avoid exceptions within finalizers since it might cause the application to crash at an undiagnizable time. Caatch all exceptions in the finalizer and report them via an alternate means than an unhandled exception.

`IDisposable` interface defines the details of the deterministic pattern with a single method `Dispose()`. `using` is identical to using try/finally block where `.Dispose()` is called in the finally block. Use `SuppressFinalize() inside `Dispose()` so that the managed resource could be garbage-collected ASAP instead of waiting for garbage-collection later. `Dispose()` should be idempotent. The `IDisposable` interface requires the implementation of a single parameterless method and a `virtual void Dispose(bool)` for any non-sealed class overload.
The disposing parameter should be false when called from a finalizer, and true when called from the IDisposable.Dispose method.

```csharp
#region IDisposable Members
  public void Dispose()
  {
      Dispose(true);

      // Unregister from the finalization queue.
      System.GC.SuppressFinalize(this);
  }
  #endregion
  public void Dispose(bool disposing)
  {
  // Do not dispose of an owned managed object (one with a
      // finalizer) if called by member finalize,
      // as the owned managed objects finalize method
      // will be (or has been) called by finalization queue
      // processing already
      if (disposing)
      {
          Stream?.Close(); // managed. Finalization will deal with it if not called deterministically
      }
      try
      {
        File?.Delete(); // unmanaged, should be dispose anyway
      }
      catch (IOException exception)
      {
        ...
      }
      Stream = null;
      File = null;
```

# Lazy Initialization

```csharp
public TypeName Instance => InternalInstance ?? (InternalInstance = CreateInstance());

private TypeName? InternalInstance { get; set; } = null;
```

Or with `System.Lazy<T>`, which provides thread-safe mechanism ensuring that only one object will be created.

```csharp
public TypeName Instance => InternalInstance.Value

private Lazy<TypeName> InternalInstance { get; } = new Lazy<TypeName>(() => CreateInstance());
```


