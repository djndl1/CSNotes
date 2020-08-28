# Platform Invoke (P/Invoke)

http://www.pinvoke.net/

Calling into API exposed by unmanaged DLLs.

```csharp

```

Since `int` is platform independent in C, we don't map them to `IntPtr` (integer that is the size of a pointer) instead of C# `int`.

pointer to `ref` or `out`

Some APIs involve types that have no corresponding managed type. Calling these types requires redeclaration of the type in managed code. Use `StructLayoutAttribute` with the declaration since managed code can optimize the memory layouts of types so layouts may not be sequential from one field to the next.

```csharp
[StructLayout(LayoutKind.Sequential)]
struct ColorRef
{
  public byte Red;
  public byte Green;
  public byte Blue;
  // Turn off warning about not accessing Unused
  #pragma warning disable 414
  private byte Unused;
  #pragma warning restore 414

  public ColorRef(byte red, byte green, byte blue)
  {
      Blue = blue;
      Green = green;
      Red = red;
      Unused = 0;
  }
}

class VirtualMemoryManager
{
    [Dllimport("kernel32.dll")]
    internal static extern IntPtr GetCurrentProcess();

    [Dllimport("kernel32.dll")]
    private static extern IntPtr VirtualAllocEx(
                                                IntPtr hProcess,
                                                IntPtr plAddress,
                                                IntPtr dwSize,
                                                AllocationType flAllocationType,
                                                uint flProtect);

    [Dllimport("kernel32.dll")]
    static extern bool VirtualProtectEx(
                                        IntPtr hProcess, IntPtr lpAddress,
                                        IntPtr dwSize, uint flnewProtect,
                                        ref uint lpflOldProtect
                                        );
)

```

 It is possible to instantiate a `System.ComponentModel.Win32Exception()` that is automatically initialized with the Win32 error data immediately following the P/Invoke call.

Use `System.Runtime.InteropService.SafeHandle` to provide a class that implementes `IDisposable` and a finalizer 

Function pointers map to delegates. https://stackoverflow.com/questions/5235445/pinvoke-c-function-takes-pointer-to-function-as-argument

# Unsafe Code

Unsafe code is an explicit code block and compilation option. `unsafe` is a directive to the compiler to permit pointer and address manipulation within the unsafe block. Furthermore, unsafe does not imply unmanaged.

When writing unsafe code, your code becomes vulnerable to the possibility of buffer overflows and similar outcomes that may potentially expose security holes. For this reason, it is necessary to explicitly notify the compiler that unsafe code occurs. To accomplish this, set the AllowUnsafeBlocks to true in your CSPROJ file.

```csharp
byte* pData; // byte is a referent type. 
byte* pData1, pData2// C# always places the * with the data type unlike in C/C++.
```

C# does not allow referent types other than unmanaged types, which are types that are not reference types, are not generics, and do not contain reference types.  Valid referent types include enums, predefined value types (`sbyte`, `byte`, `short`, `ushort`, `int`, `uint`, `long`, `ulong`, `char`, `float`, `double`, `decimal`, and `bool`), and pointer types (such as byte**). Lastly, valid syntax includes void* pointers, which represent pointers to an unknown type.

 The data can be addressed by a pointer must be classified as a variable and an unmanaged type, and classified as fixed, not movable. Use `fixed` to fix data within a certain block (the pointer variable can only be declared and used within that block).

C# allows for declaring a pointer of type `char*` and assigning it to a `string` within a fixed statement. The fixed statement prevents the movement of the string during the life of the pointer. Similarly, it allows any movable type that supports an implicit conversion to a pointer of another type, given a fixed statement.

 An alternative is to allocate the array on the call stack. Stack allocated (`stackalloc`) data is not subject to garbage collection or to the finalizer patterns that accompany it. 
