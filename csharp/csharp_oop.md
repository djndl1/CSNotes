# Class

- Place one class in a single source file.

The runtime does not manage resources such as database connections, network ports and so on. C# does support explicit deterministic resource cleanup via a `using` statement and implicit nondeterministic resource cleanup using _finalizers_.

It is a good practice to have a different naming convention for parameters and local variables than the naming convention for fields. if local variables or parameters exist with the same name as the field. Omitting this would result in accessing the local variable/parameter when the intention was the field; given this scenario, use of this is required.

## Properties

C# provides explicit syntax for getter/setter (_property_):

```csharp
class Employee
{
  // FirstName property
  public string FirstName
  {
      get
      {
          return _FirstName;
      }
      set
      {
          _FirstName = value; // value is a keyword 
      }
  }
  private string _FirstName;
                    
  // ...
}
```

Properties provides an API that looks programmtically like a field.

(C# 7.0) It is possible to declare property getters and setters using expression-bodied members:

```csharp
class Employee
{
    public string LastName
    {
        get => _FirstName;
        set => _FirstName = value;
    }
}
```

Or even simpler in need of a trivial implementation (automatically implemented properties):

```csharp
public string Title { get; set;} 
public string Salary { get; set; } = "Not enough";
```

By removing either the getter or the setter portion of a property, the property's accessibility can be changed. One important note about a read-only automatically implemented property is that, like read-only fields, the compiler requires that it be initialized via an initializer (or in the constructor).

(C# 6.0) supports _automatically implemented_ properties:

```csharp
public bool[,,] Cells { get; } = new bool[2, 3, 3];
```

The general guideline is that methods should represent actions and properties should represent data. Properties are intended to provide simple access to simple data with a simple computation. The expectation is that invoking a property will not be significantly more expensive than accessing a field. 

Property naming can be `_PascalCase`, `_camelCase` or `m_PascalCase`.

DO NOT declare instance fields that are public or protected. (Instead, expose them via a property.) DO favor automatically implemented properties over fields. DO favor automatically implemented properties over using fully expanded ones if there is no additional implementation logic.


Properties behave like virtual fields, the following is possible:

```csharp
class Employee
{
  // ...

  // FirstName property
  public string FirstName
  {
      get
      {
          return _FirstName;
      }
      set
      {
          _FirstName = value;
      }
  }
  private string _FirstName;

  // LastName property
  public string LastName
  {
      get => _LastName;
      set => _LastName = value;
  }
  private string _LastName;
  // ...

  // Name property
  public string Name
  {
      get
      {
          return $"{ FirstName } { LastName }";
      }
      set
      {
          // Split the assigned value into
          // first and last names.
          string[] names;
          names = value.Split(new char[]{' '});
          if(names.Length == 2)
          {
              FirstName = names[0];
              LastName = names[1];
          }
          else
          {
              // Throw an exception if the full
              // name was not assigned.
             throw new System. ArgumentException (
                  $"Assigned value '{ value }' is invalid", "value");
          }
      }
  }
  public string Initials => $"{ FirstName[0] } { LastName[0] }";
  // ...
}
```

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

The `this` member within a nested class refers to an instance of the nested class. A nested class can access any member on the containing class, including private members.

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

