# Class

The runtime does not manage resources such as database connections, network ports and so on. C# does support explicit deterministic resource cleanup via a `using` statement and implicit nondeterministic resource cleanup using _finalizers_.

## Properties

Properties provides an API that looks programmtically like a field. 

The general guideline is that methods should represent actions and properties should represent data. Properties are intended to provide simple access to simple data with a simple computation. The expectation is that invoking a property will not be significantly more expensive than accessing a field. 

DO NOT declare instance fields that are public or protected. (Instead, expose them via a property.) 
DO favor automatically implemented properties over fields. 


A property's getter and setter may have different accessibility (never seen in the wild).
It's possible to add an access modifier to a property:

```csharp
 public string Id
  {
      get => _Id;
      // Providing an access modifier is possible in C# 2.0
      // and higher only
      private set => _Id = value;
  }
  private string _Id;
```

- _static property_: static properties provide at least some level of encapsulation compared to `public static` field.

## Constructors

Assignments within the constructor will occur after any assignments are made when a field is declared.

_Object initializer_: initialize an object's accessible fields and properties. These initialization follows the constructor call.

```csharp
   Employee employee1 = new Employee("Inigo", "Montoya")
            { Title = "Computer Nerd", Salary = "Not enough"};
```

- _Collection initializer_: allows the assignment of items within the collection at the time of the collection's instantiation:

```csharp
      List<Employee> employees = new List<Employee>()
         {
            new Employee("Inigo", "Montoya"),
            new Employee("Kevin", "Bost")
         };
```

There are ways to eliminate the duplication by calling one constructor from another—constructor chaining—using constructor initializers. Constructor initializers determine which constructor to call before executing the implementation of the current constructor:

```csharp
  public Employee(string firstName, string lastName)
  {
      FirstName = firstName;
      LastName = lastName;
  }

  public Employee(
      int id, string firstName, string lastName )
      : this(firstName, lastName)
  {
      Id = id;
  }
```

- _static constructors_: a means to initialize the class itself. Such constructors are not called explicitly. The runtime calls static constructors automatically upon first access to the class. It can be used to initialize the static data within the class to a particular value.

Consider initializing static fields inline rather than using a static constructor, or initializing them at declaration time for performance.

## (C# 7.0) Deconstructor

```csharp
class Employee
{
     public void Deconstruct(
         out int id, out string firstName,
         out string lastName, out string salary)
     {
         (id, firstName, lastName, salary) =
              (Id, FirstName, LastName, Salary);
  }
  // ...
}

(_, firstName, lastName, salary) = employee;
```

## static class

A class with no data field should be declared as `static` to prevent instantiation. The compiler automatically marks it as `abstract` and `sealed` within the CIL, rendering it inextensible.

## Extension Methods

The creation of an instance method on a different class can simulated via _extension methods_.

```csharp
public static class DirectoryInfoExtension
{
  public static void CopyTo(
      this DirectoryInfo sourceDirectory, string target,
      SearchOption option, string searchPattern)
  {
      // ...
  }
}
  // ...
      DirectoryInfo directory = new DirectoryInfo(".\\Source");
      directory.CopyTo(".\\Target",
          SearchOption.AllDirectories, "*");
  // ...
```

As if `DirectoryInfo` has a method `CopyTo`, but actually a `static` method. However, specializing a type via inheritance is preferable to using an extension method.

## `const`, `readonly`

- In C#, `const` in a class is `static` automatically. Changing a public constant won't take effect since the constant has been compiled into the client code.

- `readonly`: modifiable only from inside the constructor or via an initializer during declaration. Use a `readonly` or automatically implemented properties for arrays to freeze the array instance. The elements of the array are still writeable.

## Nested Classes

It is possible to define a class within a class. 

Treat `public` nested classes with suspicion. 

The `this` member within a nested class refers to an instance of the nested class instead of the containing class. A nested class can access any member on the containing class, including private members.

## Partial Classes

Partial classes are portions of a class that the compiler can combine to form a complete class. The general purpose of a partial class is to allow the splitting of a class definition across multiple files.

```csharp
partial class Program
{
}
```

Another common use of partial classes is to place any nested classes into their own files, in accordance with the coding convention that places each class definition within its own file.

```csharp
// File: Program.cs
partial class Program
{
  static void Main(string[] args)
  {
    CommandLine commandLine = new CommandLine(args);

    switch (commandLine.Action)
    {
       // ...
    }
  }
}
// File: Program+CommandLine.cs
partial class Program
{
  // Define a nested class for processing the command line.
  private class CommandLine
  {
     // ...
  }
}
```

## Partial Methods

Partial methods are allowed only within partial classes, and like partial classes, their primary purpose is to accommodate code generation. Partial methods allow for a declaration of a method without requiring an implementation. Any partial method must return `void`.

In summary, partial methods allow generated code to call methods that have not necessarily been implemented. Furthermore, if there is no implementation provided for a partial method, no trace of the partial method appears in the CIL. This helps keep code size small while keeping flexibility high.


# Inheritance

The C# compiler allows an explicit conversion at compile time between potentially compatible types, the CLR will still verify the explicit cast at execution time, throwing an exception if the object instance is not actually of the targeted type.

- Defining Custom Conversions is similar to C++

```csharp
class GPSCoordiates
{
    public static implicit operator UTMCoordinates(GPSCoordiates coordinates)
    {
    
        // an implicit conversion from GPSCoordiates to UTMCoordinates
    }
}
```

Every derived class may be used as an instance of any of its base classes, an extension method on one type also extends every derived type. For the rare cases that require a multiple-inheritance class structure, one solution is to use aggregation.

- `sealed` (= Java `final`) class: a class that cannot be derived from; a method that cannot be overridden

- `virtual`: overridable, dynamic dispatch; the derived class overrides it with `override` (`override` is always used along with `virtual`). C# dispatches virtual calls to the most derived type even during instance construction (while C++ calls the base type's implementation). Java's methods are virtual/overridable by default while in C# virtual methods are explicitly declared.

- `new`: it hides a redeclared member of the derived class and cuts off the virtual call chain from the base class. If neither override nor new is specified, new will be assumed. This ensures virtual dispatch for certain derived while it provides total independence for some derived class on this method.

Programmers sometimes need to designate explicitly which base constructor to call inside the derived class constructor.

# `System.Object`

## Override `Equals(obj)`

The default implementation uses `ReferenceEquals` for reference types. 
For value types, two objects are equal if they are of the same type 
and if the values of the public and private fields of the two objects are equal.
Calling `ReferenceEquals()` on values type will always return `false` as they are in different boxes.

Typical steps:

- check for `null`

- check for equivalent types

- invoke a typed `Equal()`

- possibly check for equivalent hash codes before checking fields

- check `base.Equals()` if implemented

- comparing identifying fields

- override `GetHashCode()`

- override the `==` and `!=` operators if needed

- Never throw any exception

## Overriding `GetHashCode()`

Overriding `GetHashCode()` of a type is a good practice to be used as a key into a hash table collection.

- (required) if `a.Equals(b)` then `a.GetHashCode() == b.GetHashCode()`, the converse is not true.

- (required) For mutable reference types, override `GetHashCode()` only if the identifying fields are immutable, or the hash code does not change while the object is contained in a hash collection. Otherwise, be careful not to modify object values while the object is stored in a hash table.

- (required) `GetHashCode()` should not throw any exception.

- (for performance) unique whenever possible

- (for performance) evenly distributed

- (for performance) small difference between objects results in large difference between hash codes

- (for security) hard to craft an object of a particular hash code

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
