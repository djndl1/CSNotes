# Class

- Place one class in a single source file.

The runtime does not manage resources such as database connections, network ports and so on. C# does support explicit deterministic resource cleanup via a `using` statement and implicit nondeterministic resource cleanup using _finalizers_.

It is a good practice to have a different naming convention for parameters and local variables than the naming convention for fields. if local variables or parameters exist with the same name as the field. Omitting this would result in accessing the local variable/parameter when the intention was the field; given this scenario, use of this is required.

## Storing and Loading with Files

Instantiate a `FileStream` object and then create a `StreamWriter`/`StreamReader` for writing/reading text into/from it.

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

The general guideline is that methods should represent actions and properties should represent data. Properties are intended to provide simple access to simple data with a simple computation. The expectation is that invoking a property will not be significantly more expensive than accessing a field. 

Property naming can be `_PascalCase`, `_camelCase` or `m_PascalCase`.

DO NOT declare instance fields that are public or protected. (Instead, expose them via a property.) DO favor automatically implemented properties over fields. DO favor automatically implemented properties over using fully expanded ones if there is no additional implementation logic.
