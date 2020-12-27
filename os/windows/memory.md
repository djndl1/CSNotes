# Memory Management Overview

```
+-----------------------------------------------------+
|                                                     |
|          Windows Program     C Library              |
|                                                     |
+-----------------------------------------------------+
 +----------------------------------------------------+
 |                         ||                         |
 |  Heap API: HeapCreate.. ||                         |
 |                         ||   MMF API:              |
 +--------------------------|     CreateFileMapping   |
 +--------------------------|     CreateViewOfFile    |
 |                         ||                         |
 |  Virtula Memory API     ||                         |
 |                         ||                         |
 +----------------------------------------------------+
 +----------------------------------------------------+
 |              Windows Kernel with                   |
 |              Virutal memory manager                |
 |                                                    |
 +--------|---------------------------------^---------+
          ^                                 |
          |                                 |
          |                                 |
 +--------v-----------+            +--------v---------+
 |                    |            |                  |
 |  Physical memory   |            | Disk & filesystem|
 |                    |            |                  |
 +--------------------+            +------------------+

```

```c
VOID GetSystemInfo(LPSYSTEM_INFO lpSystemInfo)
```

# Heap

Use C library to allocate heap memory if one heap is sufficient.

Heaps are Windows objects (non-kernel objects): they have handles.

Heap separation has several benefits:

- multithreaded performance: each thread can have its own heap

- allocation efficiency and low fragmentation, simplifies synchronization

- deallocation efficiency: an entire heap can be freed with a single function call

- locality of reference efficiency, reducing page faults

Use `HeapCreate` to create a new heap, specifying the initial heap size.
Heaps are not kernel objects so they have no security attributes. Destroy a heap with `HeapDestroy`. Do not destroy the process heap.
Destroying a heap eleminate the need to write the data structure traversal code and deallocate each individual code.

`HeapAlloc`; `HeapFree`; `HeapReAlloc`; `HeapSize` (which determines the size of an allocated block)

TODO example

# Memory Mapped Files

Convenient and efficient in-memory algorithms can process file data. File processing is frequently much faster than using File I/O. No need to manage buffers. Multiple processes can share memory by mapping.

The OS itself uses memory mapping to implement DLLs and to laod and execute executables.

Create a file mapping object with `CreateFileMapping`. Get a file mapping handle with`OpenFileMapping`

TODO

# Dynamic Linked Libraries

Save memory, save disk space, no need to rebuild and dynamic resolution

1. Library functions are linked at program load time or at run time.

2. DLLs are used to create shared libraries.

3. easy to update

The entire Windows API is supported by a DLL that invokes the Windows 
kernel for additional services. Since DLLs might be used by multiple processes, it should be thread-safe.

## Implicit Linking

A `.LIB` (stubs for calling the real subroutines); `DLL` for functions; A DLL file that contains the executable image, placed in the same directory as the application.

A exported function in DLL must be annotated with `__declspec (dllexport)` (typically used in the form the `MYLIBRARYNAME_EXPORTS` macro )  or use a `.DEF` file.

The search path is 

- `.`: the application directory

- the system directory (`GetSystemDirectory`), typically `C:\Windows\System32`

- The Windows Directory `GetWindowsDirectory`

- the current directory

- `%PATH%`

## Explicit Linking (Dynamic loading) 

`LoadLibrary`, `LoadLibraryEx`; `GetModuleFileName`, `GetModuleHandle`.

The system maintains a reference count to each DLL loaded. If the DLL is implicitly linked other DLLs that cannot be located, the loading fails. 

The address of any entry point can be queried by `GetProcAddress`.

`FreeLibrary` after using it.

## The DLL Entry Point

entry point (typically `DllMain`): invoked automatically every time a process attaches or detaches the DLL or a new thread is created or terminated, specified by users.

## Versioning

1. DLL version number as part of the file names, typically used by UNIX applications.

2. side-by-side DLLs or assemblies and components. An XML manifest is added to the applications to define the DLL requirements.

`DllGetVerion`
