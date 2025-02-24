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

## Static Initialization

The initialization order of different classes is not deterministic but follows the dependency relationship between classes.
A type is first initialized before using. Their static fields are not initialized in parallel.

### Type Initialization Order

For a certain class:

1. Static fields to zero-initialized;

2. Derived type static field initializers run;

3. Base type static field initializer run;

4. Any static constructor runs, starting from the ultimate base.
   Static constructors run before any instance creation (unless any field initializer of its class creates an instance) 
   or static member access.
   
The order between static field initializers and static constructors cannot be determined 
without considering the relationships of their containing classes.
"the static constructor runs after static field initializers" applies only when initializing a certain class.
For cross-class references of static members, the static constructor of class `A` is executed 
before static members of another class retrieves the value of static members of `A`. 
In this sense, the static constructor of a class `A` runs before static field initializers of another class 
that references static memberrs of this class.

*Warning*: the order of initialization of partial classes is not defined in C#. 

### Case Analysis

```csharp
class A
{
    public static int X;

    static A()
    {
        X = B.Y + 1;
    }
}

class B
{
    public static int Y = A.X + 1;

    static B() {}

    static void Main()
    {
        Console.WriteLine($"X = {A.X}, Y = {B.Y}");
    }
}
// X = 1, Y = 2
```

- To run the static member of `B` `Main()`, `B.Y` is initialized (first to zero and then its initializer)

- `B.Y`'s initializer access `A.X`, causing `A.X`'s initializer to run (with `B.Y` having been zero-initialized) and then `A`'s static constructor

- `B.Y` is `0`, then `A.X` is initialized to `1`, then `Y = A.X + 1` is `2`

A more complex example would be:

```csharp
static class B
{
    public static int X = 7;
    static B() {
        Console.WriteLine("B.X = " + X);
        X = A.X;
        Console.WriteLine("B.X = " + X);
    }
}

static class A
{
    public static int X = B.X + 1;
    static A() {
        Console.WriteLine("A.X = " + X);
    }
}

static class Program
{
    static void Main() {
        Console.WriteLine("A = {0}, B = {1}", A.X, B.X);
    }
}
```

```shell
B.X = 7
B.X = 0
A.X = 1
A = 1, B = 0
```

- `A.X` is accessed, so its initializer is called after being zero initialization

- but `B.X` is referenced, then `B.X` is initialized to `7` and then to `A.X` (`0`), so `A.X` is finally set to `B.X + 1` (`0 + 1`)

- `B.X = 0`, `A.X = 1`

   
### Suggestions

- Use `const` instead of `static` if possible.

- Static constructors provides better maintainability and is deterministic than static field initializers if these initializers depends on each other.
  Also, they always run before dependent classes.

- For partial classes, the order of static fields initialization is undefined in different files, use static constructors instead.

- Avoid circular references between static fields (in-class or cross-class), 
  which could cause unexpected results, even stack overflows.
  
- As long as there is no circular referencing, the initialization follows the order defined by their dependency relationship.

- Consider `Lazy<T>` for expensive initialization that may not be needed at startup.

- Because the static constructor is executed exactly once for each closed constructed class type, 
  it is a convenient place to enforce run-time checks on the type parameter that cannot be checked at compile-time via constraints
  
  ```csharp
  class Gen<T> where T : struct
  {
    static Gen()
    {
        if (!typeof(T).IsEnum)
        {
            throw new ArgumentException("T must be an enum");
        }
    }
  }
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
