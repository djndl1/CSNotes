# The Problem of C++ 

C++: built on UNIX for monolithic applications using the compiler and linker technology. Libraries does not always seem to be reusable and sometimes requires the client user to understand the source code.

Traditionally, C++ libraries have been distributed in source code form and are expected to be linked into the executable code. Dynamic linking for C++ lacks standardization at the binary level, making it hard to distribute C++ libraries as DLLs: 

1. Different name mangling schemes between compiler vendors prevent dynamic linking using different compilers for client code and libraries.

2. Different compiler vendors implement language features in their own ways. e.g. exceptions.

3. C++ has no notion of binary encapsulation. The compilation of C++ requires the client's compiler to have access to all information reguard object layout in order to instantiate an instance of a class or to make nonvirtual method calls, which includes info about the size and order of the the object's privte and protected data members. Versioning on naming invites system bloat. Simply exporting C++ class definitions from DLLs does not provide a reasonable binary component architecture
