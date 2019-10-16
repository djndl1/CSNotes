# Intro

With .NET Core, all assemblies have a `.dll` extension even if they are Console programs.

In C#, it is frequently followed that the filename follows the name of the class, but this is not required. It is possible to have two classes in one file and have a single class span multiple files (_parital class_).

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

It represents a core component of the CLI implementation. It provides the representation and the behavior information about a type and includes location information about which assembly contains a particular type definition. It serves a key role in saving data from the compiler and making it accessible at execution time to debuggers and the runtime.

All header information about a library and its dependencies is found in a portion of the metadata known as the _manifest_. Metadata is available at execution time by a mechanism known as _reflection_. 

## Versioning

- C# 3.0 - M$ .NET 3.5

- C# 4.0 - M$ .NET 4

- C# 5.0 - M$ .NET 4.5

- C# 6.0 - M$ .NET 4.6

- C# 7.0 - M$ .NET 4.7


