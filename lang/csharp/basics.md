[C# for Java Developers](https://docs.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2008/ms228358(v=vs.90)?redirectedfrom=MSDN)

# Intro

With .NET Core, all assemblies have a `.dll` extension even if they are Console programs.

In C#, it is frequently followed that the filename follows the name of the class, but this is not required. It is possible to have two classes in one file and have a single class span multiple files (_partial class_).

## Syntax Fundamentals

[Design Guidelines](https://docs.microsoft.com/zh-cn/dotnet/standard/design-guidelines/index?redirectedfrom=MSDN)

Identifiers identify constructs.

- No use of abbreviations or contractions in identifier names. Use acronyms only when they are well established.

- No underscores, hyphens, or other nonalphanumeric characters in identifier names.

The designers of C# reserve the right to make any identifier that begins with two underscores into a keyword in a future version.

- Type name by convention must be PascalCased.

the `Main` method returns either `void` or `int` and takes either no parameters or a single array of strings, which contains command-line parameters after the exectable name (use `System.Environment.CommandLine` to retrieve the full command).

Local variable names are camelCase and do not include underscores. C# requires that local variables be determined by the compiler to be “definitely assigned” before they are read.

Console I/O is done through `System.Console`


- string interpolation:

```csharp
string firstName = "Hans";
System.Console.WriteLine($"The first name is {firstName}");
```

or we can use _composite formatting_, which requires a format string:

```csharp
System.Console.WriteLine("The first name is {0}", firstName);
```

```csharp
/** XML delimited comments **/
/// XML single line comments
```

Code that is readable without comments is more valuable than that which requires comments to clarify what it does. If developers find it necessary to enter comments to clarify what a particular code block is doing, they should favor rewriting the code more clearly over commenting it.

## Managed Execution and The Common Language Infrastructure

Assemblies consist mainly of Common Intermediate Language (CIL), or IL for short. The C# compiler transforms the C# source file into this intermediate language. The _Virtual Execution System_ (VES), also casually referred to as the _runtime_, JIT-compiles (jitting) CIL code as needs. The code executed under the context of an agent such as the runtime is termed _managed code_. Otherwise, it's native code (unmanaged code).

The CLI includes specifications for the VES, the CIL, a type system (Common Type System, CTS) that supports language interoperability, guide on how to write libraries for CLS languages and metadata that enables many of the services identified by the CLI.

On mono, `monodis` is the disassebler. Several obfuscator exist to decrease accessibility to the source code.



### The Common Language Infrastructure

The specifications for the CIL and the runtime are contained within an international standard known as the _Common Language Infrastructure_.

Contained within the CLI standard are specifications for the following:

- The Virtual Execution System, more casually known as Common Language Runtime

- The Common Intermediate Language

- The Common Type System

- The Common Language Specification

- Metadata

- The framework (APIs)

CLI Implementations:

- .NET Core: __THE FUTURE!!!__. High-performance and portable. The core functionality common to all new implementations of .NET. The CoreCLR is the implementation of the CLR for .NET Core.  Core CLR, Core framework libraries, command-line tools. Its API is compatible with existing .NET Framework, Xamarin and Mono via .NET Standard.

- Microsoft .NET Framework: WPF (GUI framework), WF, ASP.NET (web sites and web-based APIs), .NET Framework Base Class Library (types representing the built-in CLI data types), with the largest API set.

- Microsoft Silverlight: obsolete, 

- .NET Micro Framework: Microsoft's open source implementation of the CLI for small devices

- .NET Compact Framework: obsolete, a trimmed-down implementation of the .NET Framework designed for PDAs, phones and the XBox 360.

- Xamarin: highly portable

- Mono: Unix worlds and game consoles

- DotGNU Protable .NET: decommissioned

- Shared Resource CLI: reference implementations of the CLI for noncommercial use.

.NET Standard emerged as a means of defining what APIs are supported by which version of the standard. From .NET standard 2.0, all the base frameworks have or are working towards implementing this standard.

_Base Class Library (BCL)_: The CLI defines a core set of class libraries, which provides foundational types and APIs.

#### Compilation

- JIT

- interpretation

- AOT: The .NET Native feature, supported by .NET Core and recent .NET Framework implementations, creates a platform-specific executable. When .NET Native compiles an application, the .NET Framework Class Library is statically linked the application; .NET Framework runtime components optimized for static pre-compilation are included as well.

#### Runtime

Even after the runtime converts the CIL code to machine code and starts to execute it, it continues to maintain control of the execution. Because an agent controls program execution, it is possible to inject additional services into a program, even though programmers did not explicitly code for them.

- Garbage Collection: The garbage collector takes responsibility only for handling memory management; that is, it does not provide an automated system for managing resources unrelated to memory. Most implementations of the CLI use a generational (objects that have lived for only a short period will be cleaned up sooner than objects that have already survived garbage collection sweeps because they were still in use.), compacting, mark-and-sweep–based algorithm (During each garbage collection execution, it marks objects that are to be de-allocated and compacts together the objects that remain so that there is no “dirty” space between them.) to reclaim memory. TODO

- type safety: the runtime offers type conversions checking.

#### Performance

Microsoft has indicated that managed development will be the predominant means of writing applications for its Windows platform in the future, even for those applications that are integrated with the operating system. As managed execution increases in capability and sophistication, many of these performance considerations will likely vanish.

The runtime introduces several factors that can contribute to improved performance over native compilation:

- machine-dependent optimization

- allocate memory according to system usage

CLI programs are not necessarily faster than non-CLI programs, but their performance is competitive.

#### Assemblies, manifests, and modules

An assembly includes a _manifest_ that is made up of:

- the types that an assembly defines and imports;

- version information

- additional files that the assembly depends only

- security permissions for the assembly

Essentially a header to the assembly.

Assemblies can be class libraries or the executables themselves.

An assembly contains the CIL code within one or more modules. Generally the assembly and the manifest are combined into a single file. However, it is possible to place modules into their own separate files and then use an assembly linker to create an assembly file that includes a manifest that references each module. However, an assembly are the smallest versioned unit. Assemblies form the logical construct of a component or unit of deployment.

#### Common Intermediate Language

The CLI is to support the interaction of multiple languages within the same application (instead of portability of source code across multiple operating systems).

#### Common Type System

The CTS defines how types are structured and laid out in memory, as well as the concepts and behaviors that surround types. It is the responsibility of the runtime at execution time to enforce the contracts established by the CTS.

Types are classfied into:

- Values: bit patterns used to represent basic types;

- Objects

#### Common Language Specification

The CLS is intended for library developers, and provides them with standards for writing libraries that are accessible from the majority of source languages, regardless of whether the source languages using the library are CTS-compliant.

#### Metadata

CIL code includes metadata about the types and files included in a program.

- description of each type within a program or class library;

- custom attributes embedded in the code

- the manifest information containing data about the program itself.

It represents a core componen
t of the CLI implementation. It provides the representation and the behavior information about a type and includes location information about which assembly contains a particular type definition. It serves a key role in saving data from the compiler and making it accessible at execution time to debuggers and the runtime.

All header information about a library and its dependencies is found in a portion of the metadata known as the _manifest_. Metadata is available at execution time by a mechanism known as _reflection_. 

## Versioning

- C# 3.0 - M$ .NET 3.5

- C# 4.0 - M$ .NET 4

- C# 5.0 - M$ .NET 4.5

- C# 6.0 - M$ .NET 4.6

- C# 7.0 - M$ .NET 4.7


# Data Types

## Predefined types

All the fundamental types in C# have both a short name and a BCL name, which is the same across all languages: `System.typeName`

- integer types: `sbyte`, `byte` (8-bit); `short`, `ushort` (16-bit); `int`, `uint` (32-bit), `123u`; `long`, `123l`, `123L`, `ulong` (64-bit), `123ul`; 

- floating-point type: [`float` (32-bit)](https://en.wikipedia.org/wiki/Single-precision_floating-point_format) `8F` ; [`double` (64-bit)](https://en.wikipedia.org/wiki/IEEE_754); there are two properties for a floating-point number: precision and range. For an integer, they are the same.

```python
# double
>> sum = 0
>> for i in range(1,52):
        sum += (1/2)**i

>> 2**1023*(1+sum)
1.7976931348623155e+308
```

```csharp
string.Format("{0:R}", 1.61234168471843814783718431); // round-trip format

const double number = 1.618033988749895;
double result;
string text;

text = $"{number}";
result = double.Parse(text);
System.Console.WriteLine($"{result == number}: result == number");

text = string.Format("{0:R}", number);
result = double.Parse(text);
System.Console.WriteLine($"{result == number}: result == number");
```

```bash
False: result == number
True: result == number
```

- decimal float: `decimal` (128-bit), `120M`; maintains exact accuracy for all denary number within the range. Doesn't follow IEEE-754

- boolean types: `bool`, (8-bit)

- character type: `char`, (16-bit), UTF-16, too small for all unicode characters, sometime require two `char`s.

- `_`: digit separator; `9_814_072_356`.

- `e`/`E`: exponent notation is available: `6.023E23`

- `ob`: binary notation is supported

```csharp
System.Console.WriteLine($"0x{42:X}");
```

## String Type

- `string`: `@` verbatim string (raw string except for `"`), `$` string interpolation (both can be combined together). The both can be combined. String interpolation is a shorthand for invoking the `string.Format()` method. Strings are immutable.

```csharp
System.Console.WriteLine(@"\nThis is not escaped!");

System.Console.Write(@"begin
             /\
            /  \
           /    \
          /      \
         /________\
end");
```

```bash
\nThis is not escaped!
begin
             /\
            /  \
           /    \
          /      \
         /________\
end
```

Important Static Methods

- `string.Format()`

- `string.Concat()`

- `string.Compare()`

`using static` directive allows access static members and nested types of a type

Rely on `System.WriteLine()` and `System.Environment.NewLine` rather than ‘\n’ in order to accommodate Windows specific operating system idiosyncrasies with the same code that runs on Linux and iOS.

Use `System.Text.StringBuilder` to construct a long string in multiple steps.

- `null`: the variable does not refer to any valid object. Reference types, pointer types and nullable value types can be assigned the value `null`.

C# provides checked block and uncheked block to decide what should happen if the target data type is too small to contain the assigned data.

Each numeric data type includes a `Parse()`/`TryParse()` function that enables conversion from a string to the corresponding numeric type.

```csharp
float kgElectronMass = float.Parse("9.11E-31");
```

Also `System.Convert`:

```csharp
double middleC = System.Convert.ToDouble("261.626");
```



```csharp
// double number;
string input;

System.Console.Write("Enter a number: ");
input = System.Console.ReadLine();
if (double.TryParse(input, out double number))
{
    System.Console.WriteLine(
        $"input was parsed successfully to {number}."); }
else
{

    // Note: number scope is here too (although not assigned)
    System.Console.WriteLine(
        "The text entered was not a valid number.");
}
```

All types fall into: 

- value types:

- reference types: in practice, a refrence is always the same size as the native size of the processor.

`?` modifier declares a variable as nullable, which represents values that are missing for a value type. It is useful for database programming.

```csharp
int? count = null;
do
{
/...
}
while (count == null);
```

`var`: declaring an implicitly typed local variable. It was added mainly to permit use of anonymous types.

```csharp
      var patent1 =
          new { Title = "Bifocals",
          YearOfPublication = "1784" };
```

### (C# 7.0) Tuples

Tuples are a light-weight solution for encapsulating data into a single object.

```csharp
(string country, string capital, double gdppercapita) =
            ("Malawi", "Lilongwe", 226.50);
            
string country, capita;
double gdppercapita;
(country, capita, gdppercapita) = ("Malawi", "Lilongwe", 226.50);

var (country, capital, gdppercapita) = ("Malawi", "Lilongwe", 226.50);

(string country, string capital, double gdppercapita) info =
            ("Malawi", "Lilongwe", 226.50);
            
var info = (Name: "Malawi", Capita, "Lilongwe", Gdppercapita: 226.50);
// info.Name, info.Capita, info.Gdppercapita

var info = ("Malawi", "Lilongwe", 226.50);
// info.Item1, info.Item2, info.Item3

(string name, _, double gdppercapita) countryInfo = ("Malawi", "Lilongwe", 225.50);
var countryInfo = (country, capital, gdpPerCapita);
```

`System.ValueType` is the underlying implementation for the tuple syntax. The custom names are known by the compiler through the scope where these names are declared. The compiler looks at the item names within the tuple declaration and leverages those to allow code that uses those names within the scope. For all types that are part of the API (such as the return type), the compiler adds item names to the metadata of the member in the form of attributes.

Tuples have more than seven parameter takes a subtuple for the rest of the parameters. `System.valueTuple.Create()` was used before C# 7.0 tuple syntax.

### Arrays

Most programs now use generic collection types rather than arrays when storing collections of data.

```csharp
int[,] cellsOne; // 2D
cellsTwo = {
    {1, 0, 2},
    {1, 2, 0},
    {1, 2, 1}
};
int[,] cellsTwo = int[3,3];

```

- `default`: explicit default of any data type

```csharp
int count = default(int)
```

`new` may specifies the size of an array within the square brackets

```csharp
string[] languages = new string[9]{
    "C#", "COBOL", "Java",
    "C++", "Visual Basic", "Pascal",
    "Fortran", "Lisp", "J#"};
```

Assigning an array but not initializing the initial values will still initialize each element.

It is possible to specify the size at runtime

```csharp
string[] groceryList;
System.Console.Write("How many items on the list? ");
int size = int.Parse(System.Console.ReadLine());
groceryList = new string[size];
```

It is also possible to define a jagged array, which is an array of arrays.

```csharp
int[][] cells = {
    new int[]{1, 0, 2, 0},
    new int[]{1, 2, 0},
    new int[]{1, 2},
    new int[]{1}
};

int[][] cells = {
    new int[]{1, 0, 2},
    new int[]{0, 2, 0},
    new int[]{1, 2, 1}
};
```

Note the difference between a multi-dimensional array and a jagged array. (this is different from in Java, where there is only arrays of arrays).

Arrays include additional methods for manipulating the elements within the array—for example, `Sort()`, `BinarySearch()`, `Reverse()`, and `Clear()`, through `System.Array`

Variables of type `string` are accessible like an array of characters. 

[Equality Operators](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/equality-operators)

# Operators

Operands are always evaluated from left to right in C#. For `A() + B() * C()`,`A()` is first evaluated then `B()` and finally `C()`, in contrast to C/C++. `M(x++, x++)` with `x=1` is always `M(1,2)`.

# Control Flow

In C#, a local variable is in scope throughout the entire block in which it is declared, but it is illegal to refer to the local variable before its declaration (there might be another variable with the same ).

The following is illegal in C# while totally fine with C++:

```csharp
int a = 5;
{
    a = 6;
    int a = 8;
}
```

- null-coalescing operator: `expr1 ?? exp
r2`, if `expr1` is null, use `expr2`

```csharp
string fileName = GetFileName();
string fullName = fileName ?? "default.txt";
```

(C# 6.0) - null-conditional operator `?.`: checks whether the operand is null prior to invoking the method or property.

```csharp
args?.Length // this is equivalent to
(args != null) ? (int?)args.Length : null
```

Null-conditional operators can also be used in combination with an index operator

```csharp
// not of much use though
      string directoryPath = args?[0];
      string searchPattern = args?[1];
```

- foreach loop

```csharp
foreach(type variable in collection) // `variable` is read-only
  statement
```

For a switch-clause, C# does not allow control to accidentally fall through from one switch section to the next. To force a similar behavior, use `goto`. C# 7.0 introduced an improvement to the switch statement that enabled pattern matching.

 C# supports `goto`, and it is the only method for supporting fall-through within a switch statement.

```csharp
switch (option)
      {
          case "/out":
              isOutputSet = true;
              isFiltered = false;
              goto default;
          case "/f":
              isFiltered = true;
              isRecursive = false;
              goto default;
          default:
          ...
      }
```

To branch to a switch section label other than the default label, use the syntax `goto case constant`;, where `constant` is the constant associated with the case label you wish to branch to.

C# prevents using `goto` to branch into a code block; instead, `goto` may be used only to branch within a code block or to an enclosing code block. 

# C# Preprocessor Directives

compile-time control

```csharp
#line org-lien new-line

#region pre-proc-message
    code
#endregion

#pragma warning disable
#pragma warning restore
```

# Namespace

A hierarchical categorization mechanism for grouping all types related to a particular area of functionality.

Almost all C# programs use types within the `System` namespace.

- `using statc`

- `using` directive

- `using` aliasing: `using CountDownTimer = System.Timers.Timer;`

# Method


Parameter names use camelCase.

A method can only have one return type, the return type could be a tuple. It is possible to return multiple values packaged as a tuple using C# tuple syntax.

```csharp
  static (string First, string Last) GetName()
  {
      string firstName, lastName;
      firstName = GetUserInput("Enter your first name: ");
      lastName = GetUserInput("Enter your last name: ");
      return (firstName, lastName);
  }
```

- expression bodied methods: 

```csharp
static string GetFullName( string firstName, string lastName) =>
        $"{ firstName } { lastName }";
```

If a program includes two classes with `Main()` methods, it is possible to specify on the command line which class to use for the `Main()` declaration.

## Parameter Passing and Return Value

- Arguments to method calls are usually passed by value. `ref` in the parameter list makes a reference parameter.

```csharp
  static void Swap(ref string x, ref string y)
  {
      string temp = x;
      x = y;
      y = temp;
  }
```

When the called method specifies a parameter as ref, the caller is required to supply a variable, not a value, as an argument, and to place ref in front of the variables passed.

- output parameter `out`: it is often the case that a method that takes a reference to a variable intends to write to the variable, but not to read from it. An out parameter is functionally identical to a ref parameter. Whenever a parameter is marked with out, the compiler will check that the parameter is set for all code paths within the method that return normally.

A common coding error when working with out parameters is to forget to declare the `out` variable before you use it. (C# 7.0) `out` modifier allows a variable to be declared on the spot when using it as an out argument. Also, it is possible to discard an `out` parameter entirely by using an underscore as the `out` variable. 

```csharp
class ConvertToPhoneNumber
{
  static int Main(string[] args)
  {
      if(args.Length == 0)
      {
          Console.WriteLine(
              "ConvertToPhoneNumber.exe <phrase>");
          Console.WriteLine(
              "'_' indicates no standard phone button");
          return 1;
      }
      foreach(string word in args)
      {
          foreach(char character in word)
          {
              if(TryGetPhoneButton(character, out char button))
              {
                  Console.Write(button);
              }
              else
              {
                  Console.Write('_');
              }
          }
      }
      Console.WriteLine();
      return 0;
  }

  static bool TryGetPhoneButton(char character, out char button)
  {
      bool success = true;
      switch( char.ToLower(character) )
      {
          case '1':
              button = '1';
              break;
          case '2': case 'a': case 'b': case 'c':
              button = '2';
              break;

          // ...

          case '-':
              button = '-';
              break;
          default:
                // Set the button to indicate an invalid value
                button = '_';
              success = false;
              break;
      }
      return success;
  }
}
```


- (C#7.0) Support for returning a reference to a variable.

```csharp
public static ref byte FindFirstRedEyePixel(byte[] image)
{
  // Do fancy image detection perhaps with machine learning.
  for (int counter = 0; counter < image.Length; counter++)
  {
    if(image[counter] == (byte)ConsoleColor.Red)
    {
      return ref image[counter];
    }
  }
  throw new InvalidOperationException("No pixels are red.");
}
```

Only the following can be returned from a reference-returning function:

- references that were passed in as parameters to the by-reference-returning function;

- other referencing-returning properties or functions;

- references to fields or array elements;

 `ref` locals are initialized to refer to a particular variable, and can’t be modified to refer to a different variable. When declaring a reference local variable, initialization is required.
 
 ```csharp
 ref int rbyref(ref int c)
    {
        int a = 5;
        ref int b = ref a;
        return ref c;
    }
 ```

A by-reference-returning method must return a reference, even if it does not need to. The only workaround is to throw an exception.

Declaring a field of type ref isn’t allowed.

- `params` array (variadic parameters): sometimes it is convenient if the number of arguments may vary. 

```csharp
static string Combine(params string[] paths)
  {
      string result = string.Empty;
      foreach (string path in paths)
      {
          result = Path.Combine(result, path);
      }
      return result;
  }
```

If the target method implementation requires a minimum number of parameters, those parameters should appear explicitly within the method declaration, forcing a compile error instead of relying on runtime error handling if required parameters are missing. 

```csharp
int Max(int first, params int[] operands) // rather than as
int Max(params int[] operands)
```


## Method Overloading


Method overloading is a type of operational polymorphism.

```csharp
using System.IO;

public static class LineCounter
{
  public static void Main(string[] args)
  {
      int totalLineCount;

      if (args.Length > 1)
      {
          totalLineCount =
              DirectoryCountLines(args[0], args[1]);
      }
      if (args.Length > 0)
      {
          totalLineCount = DirectoryCountLines(args[0]);
      }
      else
      {
          totalLineCount  = DirectoryCountLines();
      }

      System.Console.WriteLine(totalLineCount);
  }

  static int DirectoryCountLines()
  {
      return DirectoryCountLines(
          Directory.GetCurrentDirectory());
  }

  static int DirectoryCountLines(
      string directory, string extension = "*.cs") // optional/default parameter
  {
      int lineCount = 0;
      foreach (string file in
          Directory.GetFiles(directory, extension))
      {
          lineCount += CountLines(file);
      }

      foreach (string subdirectory in
          Directory.GetDirectories(directory))
      {
          lineCount += DirectoryCountLines(subdirectory);
      }

      return lineCount;
  }

  private static int CountLines(string file)
  {
      int lineCount = 0;
      string line;
      FileStream stream =
          new FileStream(file, FileMode.Open);5
      StreamReader reader = new StreamReader(stream);
      line = reader.ReadLine();
      while(line != null)
      {
          if (line.Trim() != "")
          {
              lineCount++;
          }
          line = reader.ReadLine();
      }

      reader.Close();  // Automatically closes the stream
      return lineCount;
  }
}
```

Developers implement only the core logic in one method and all the other overloaded methods will call that single method.


## Named Arguments

Instead of positional parameters, it is possible for the caller to explicitly identify the name of the parameter to be assigned a value.

```csharp
class Program
{
  static void Main()
  {
      DisplayGreeting(
          firstName: "Inigo", lastName: "Montoya");
  }

  public static void DisplayGreeting(
      string firstName,
      string middleName = default(string),
      string lastName = default(string))
  {

      // ...

  }
}
```

With the addition of named parameters, the parameter name becomes part of the interface because changing the name would cause code that uses the named parameter to no longer compile.

When the compiler must choose which of several applicable methods is the best one for a particular call, the one with the “most specific” parameter types is chosen. If two methods are applicable because one of them has an optional parameter, the compiler will resolve to the method without the optional parameter. 

# Exception

```csharp
   try
      {
          age = int.Parse(ageText);
          Console.WriteLine(
              $"Hi { firstName }! You are { age*12 } months old.");
      }
      catch (FormatException )
      {
          Console.WriteLine(
              $"The age entered, { ageText }, is not valid.");
          result = 1;
      }
      catch(Exception exception)
      {
          Console.WriteLine(
              $"Unexpected error:  { exception.Message }");
          result = 1;
      }
      finally
      {
          Console.WriteLine($"Goodbye { firstName }");
      }
      

```

As soon as the runtime determines that the exception will be unhandled, it checks whether a debugger is installed on the machine, because you might be the software developer who is analyzing this failure. If a debugger is present, it offers the user the chance to attach the debugger to the process before the finally block runs. If there is no debugger installed or if the user declines to debug the problem, the default behavior is to print the unhandled exception to the console, and then see if there are any finally blocks that could run.the runtime is not required to run finally blocks in this situation; an implementation may choose to do so or not. 

All objects thrown as exceptions derive form `System.Exception`. 

It is possible to specify a catch block that takes no parameters (general catch block). It is equivalent to speicifying a catch block that takes an `object` data type. Following a System.Exception catch block with a general catch block in C# 2.0 or later will result in a compiler warning indicating that the general catch block will never execute. AVOID general catch blocks and replace them with a catch of `System.Exception`.
