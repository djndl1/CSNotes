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
