# The Problem of C++ 

C++: built on UNIX for monolithic applications using the compiler and linker technology. Libraries does not always seem to be reusable and sometimes requires the client user to understand the source code.

Traditionally, C++ libraries have been distributed in source code form and are expected to be linked into the executable code. Dynamic linking for C++ lacks standardization at the binary level, making it hard to distribute C++ libraries as DLLs: 

1. Different name mangling schemes between compiler vendors prevent dynamic linking using different compilers for client code and libraries.

2. Different compiler vendors implement language features in their own ways. e.g. exceptions.

3. C++ has no notion of binary encapsulation. The compilation of C++ requires the client's compiler to have access to all information reguard object layout in order to instantiate an instance of a class or to make nonvirtual method calls, which includes info about the size and order of the the object's privte and protected data members. Versioning on naming invites system bloat. Simply exporting C++ class definitions from DLLs does not provide a reasonable binary component architecture

4. Interface encapsulation does not work at the binary level, especially vtable. (Assuming all compilers on a given platform implement the virtual function call mechanism in the same manner) One way to implement an interface is to use _Pimpl_ (pointer to implementation) as a class member. The C++ interface class should not contain any of the data members that will be used in the implementation of the object. The interface class simply contains an opaque pointer whose type would never be fully defined in the  client's scope. This ensures that the size of the interface class never changes. Forward declaration means the class definition for the implementation is hided from the client's compiler. Pure virtual abstract class as an interface requires its derived class to expose its class definition unless using a global factory method. All this still does not save binary compatibility since the destructor must be virtual and thus pollutes the compiler independence of the interface class. A _Delete_ method must be introduced and explicitly called after using the object. Virtual calls are immuned to name mangling since they are called through function pointers and the factory method is exposed as a C function. Combined with dynamic loading, this creates a runtime polymorphism.
Directly extending the interface by adding another virtual function breaks compatibility. The solution is to add an extension interface. `dynamic_cast<>()` should be prohibited here as it is dependent on compiler implemention in favor of a semantically equivalent virtual function `Dynamic_Cast()` (which navigates the type hierarchy and `static_cast` the pointer. An old implementation will return a null pointer for a new interface). Note that casting to virtual base is another binary compatibility breaker. Resource management can be done via explicit manual refcounting. The client simply calls `DuplicatePointer`/`DestroyPointer`, which can be easily wrapped into a smart pointer.

# Intefaces

Use an indepedent interface description language: IDL, which generates C/C++ headers (type definitions), type library for other languges. _Attributes_ (annotations) precede the definition of the subjet of the attribute.

Method defintions in IDL are simply annoted C function prototypes.

Virtually all COM methods return an error number of type `HRESULT` (32-bit signed integer), which is partitioned by bit into Severity-Reserved-Facility-Information. `MAKE_HRESULT` is there to define a custom `HRESULT`.

```idl
HRESULT Method2([in] short arg1,
                [out, retval] short *parg2);
```

```cpp
virtual HRESULT __stdcall Method2(short arg1)
```

The interface definition has _the interface name_, _the base interface name_, _the interface body_, and _the interface atrributes_.

```idl
[object, uuid(...)]
interface IThisInterface : IBaseInterface {
    typedef1;
    typedef1;
    ...
    method1;
    method2;
    ...
}
```

UUIDs are the runtime representations of interface names.

`IUnknown` is the root of all COM interfaces. Every other legal COM interface must derive from `IUnknown` directly or indirectly. COM interfaces cannot derive directly from more than one interface.

Resource management of `IUnknown` is straightforward:

1. Call `AddRef` when a non-null interface pointer is copied.

2. Call `Release` prior to overwriting memory location that contains a non-null interface pointer.

3. Redundant calls to `AddRef` and `Release` can be optimized away if there is special knowledge about the relationship between two or even more memory locations.

The return refcount by `AddRef` and `Release` are not thread-safe, only for debugging. `Release` does not nullify the pointer, so the object might still be valid, even if it shouldn't be used.
