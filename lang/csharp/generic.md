C# generics are very different from both Java generics and C++ templates in the details of their implementation and impact upon the type system of their respective languages. Generics using type erasure has a type safety problem and seriously affects performance when dealing with value types, which boxes and unboxes data. Generics improves type safety and perfromance (no casting, boxing and unboxing), eleminates code bloat.

Every generic type must be of a specific arity. There is no support in the CLR type system for variadic generic types.

Generic support the ability to define constraints on type: interface, class type (derived, class type constraints must appear before any interface type constraints, multiple base class constraints are not allowed since it is not possible to derive from multiple unrelated classes.), `struct` (non-nullable value type)/`class` (reference type), public default construct constraint (`new()`), 

```csharp
public class BinaryTree<T>
    where T: System.IComparable<T>
{
    public T Item { get; set; }
    public Pair<BinaryTree<T>> SubItems
    {
        get{ return _SubItems; }
        set
        {
            IComparable<T> first;
            // Notice that the cast can now be eliminated.
            
            first = value.First.Item;
                    
            if (first.CompareTo(value.Second.Item) < 0)
            {
                // first is less than second.
                ...
            }
            else
            {
                // second is less than or equal to first.
                ...
            }
            _SubItems = value;
        }
    }
    private Pair<BinaryTree<T>> _SubItems;
}
```

Neither generic type parameters nor their constraints are inherited by a derived class. The type parameters must have equal (or stronger) constraints as those on the base class. In contrast to the situation with type parameters declared on a generic class, constraints on overriding virtual generic methods (or explicit interface) methods are inherited implicitly and may not be restated. In the generic class inheritance case, the type parameter on the derived class can be further constrained by adding not only the constraints on the base class (required), but also other constraints. However, overriding virtual generic methods need to conform exactly to the constraints defined by the base class method. Additional constraints could break polymorphism, so they are not allowed and the type parameter constraints on the overriding method are implied.

Delegate types, array types, and enumerated types may not be used as class type constraints, because they are effectively sealed types.

For generic methods, the compiler can infer the type parameters from the formal parameters passed to the method. Contradictory inferred types result in a common type, instead of an error.

Covariance TODO

# Internals

The implementation of generics is different for value-based type parameters than for generics with reference type parameters. When a generic type is first constructed with a value type as a type parameter, the runtime creates a specialized generic type with the supplied type parameter(s) placed appropriately in the CIL (like in C++).

Generics work slightly differently for reference types. The first time a generic type is constructed with a reference type, the runtime creates a specialized generic type with `object` references substituted for type parameters in the CIL, not a specialized generic type based on the type argument. Each subsequent time a constructed type is instantiated with a reference type parameter, the runtime reuses the previously generated version of the generic type, even if the reference type is different from the first reference type. To still gain the advantage of type safety, for each object reference substituted in place of the type parameter, an area of memory for an Order type is specifically allocated and the pointer is set to that memory reference.

Beyond the inclusion of the arity and type parameter in the class header and the type parameter denoted with exclamation points in code, there is little difference between the CIL generated for a generic class and the CIL generated for a nongeneric class.


https://stackoverflow.com/questions/31876372/what-is-reification

https://en.wikipedia.org/wiki/Comparison_of_C_Sharp_and_Java#Type_erasure_versus_reified_generics
