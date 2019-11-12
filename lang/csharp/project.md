C# and the underlying framework allow code to spread across multiple assemblies.

By default, a class without any access modifier is defined as `internal`. To expose a class outside the assembly, the assembly must be marked as public. The internal access modifier is not limited to type declarations; that is, it is also available on type members. Consequently, you can designate a type as public but mark specific methods within the type as internal so that the members are available only from within the assembly. Members with an accessibility modifier of `protected internal` will be accessible from all locations within the containing assembly and from classes that derive from the type, even if the derived class is not in the same assembly.

In the CLR, there is no such thing as a "namespace". It is helpful to create a folder for each namespace, placing a class such as `AddisonWesley.Fezzik.Services.Registration` into a folder hierarchy corresponding to the name.
