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

# Variance (category theory)

_Covariance_:  uppose two types X and Y have a special relationship—namely, that every value of the type X may be converted to the type Y. If the types I<X> and I<Y> always also have that same special relationship, we say, “I<T> is covariant in T.”

- `out`: a type is only used as the return type of a method or read-only property. Indicates that a generic interface is intended to be covariant in one of its type parameters.

```csharp
Pair<Contact> contacts =
          new Pair<Contact>(
              new Contact("Princess Buttercupt"),
              new Contact("Inigo Montoya") );
      IReadOnlyPair<PdaItem> pair = contacts;
      PdaItem pdaItem1 = pair.First;
      PdaItem pdaItem2 = pair.Second;
      // read-only to prevent another pdaItem type from being assigned to pdaItem1 
```

Only generic interfaces and generic delegates may be covariant.the varying type arguments of both the source and target generic types must be reference types. The compiler must be able to verify that the annotated type parameters are used in only "output" parameters.

 _Contravariance_: suppose two types X and Y are related such that every value of the type X may be converted to the type Y. If the types I<X> and I<Y> always have that same special relationship “backward”—that is, every value of the type I<Y> can be converted to the type I<X>—we say, “I<T> is contravariant in T.”
 e.g. an apple is a fruit, a comparator that compares two fruits can also compare two apples, i.e. comp<fruit> is a comp<apple> but not the inverse.

`in`: only as a formal parameter or writeable property type.  This instructs the compiler to check that T never appears on a property getter or as the return type of a method, thereby enabling contravariant conversions for this interface.

```csharp
      ICompareThings<Fruit> fc = new FruitComparer();
      Apple apple1 = new Apple();
      Apple apple2 = new Apple();
      Orange orange = new Orange();
      // A fruit comparer can compare apples and oranges:
      bool b1 = fc.FirstIsBetter(apple1, orange);
      // or apples and apples:
      bool b2 = fc.FirstIsBetter(apple1, apple2);
      // This is legal because the interface is
      // contravariant
      ICompareThings<Apple> ac = fc;
      // This is really a fruit comparer, so it can
      // still compare two apples
      bool b3 = ac.FirstIsBetter(apple1, apple2);
```

Say comp<fruit> returns a `fruit`, but a `fruit` isn't necessarily an `apple`, thus allowing `out` would violate type safety. 

 The compiler will check the validity of the covariance and contravariance type parameter modifiers throughout the source.

C# support array covariance, though not type-safe.  Every array is convertible to the read-only (and therefore safely covariant) interface IEnumerable<T>; that is, IEnumerable<Fruit> fruits = new Apple[10] is both safe and legal because there is no way to insert an Orange into the array if all you have is the read-only interface.

# Internals

The implementation of generics is different for value-based type parameters than for generics with reference type parameters. When a generic type is first constructed with a value type as a type parameter, the runtime creates a specialized generic type with the supplied type parameter(s) placed appropriately in the CIL (like in C++).

Generics work slightly differently for reference types. The first time a generic type is constructed with a reference type, the runtime creates a specialized generic type with `object` references substituted for type parameters in the CIL, not a specialized generic type based on the type argument. Each subsequent time a constructed type is instantiated with a reference type parameter, the runtime reuses the previously generated version of the generic type, even if the reference type is different from the first reference type. To still gain the advantage of type safety, for each object reference substituted in place of the type parameter, an area of memory for an Order type is specifically allocated and the pointer is set to that memory reference.

Beyond the inclusion of the arity and type parameter in the class header and the type parameter denoted with exclamation points in code, there is little difference between the CIL generated for a generic class and the CIL generated for a nongeneric class.


https://stackoverflow.com/questions/31876372/what-is-reification

https://en.wikipedia.org/wiki/Comparison_of_C_Sharp_and_Java#Type_erasure_versus_reified_generics
