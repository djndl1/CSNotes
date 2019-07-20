#  The `Comparator` Interface

An interface that can be used as a comparator. It has no state, but still needs to be instantiated.

```java
public interface Comparator<T> 
{
   int compare(T first, T second);
}

class LengthComparator implements Comparator<String> 
{
   public int compare(String first, String second) 
   {
      return first.length() - second.length();
   }
}

String[] friends = {"Peter", "Paul" , "mary"};
Arrays.sort(friends, new LengthComparator());
```

# The `Cloneable` interface (C++ copy assignment, copy constructor )

The `Cloneable` interface indicates that a class has provided a safe `clone()` method. The default `clone` of the `Object` class copies only every fields of an object and is `protected`. The default cloning operation is _shallow_: it does not copy objects that are referenced inside other objects. If the object inside is _immutable_, shallow cloning is enough.

However, subobjects are often mutable, which requires redefinition of the `clone` method to make a deep copy.

For every class, it is necessary to decide whether decide whether

- The default `clone` method is good enough.

- Patch up the default `clone` method by calling clone on the mutable subobjects

- `clone` method should not be attempted, which is actually the default.

An subclass can call a `protected` `clone` method only to clone its own objects. Thus `clone` must be redefined to be public 

Since `clone()` method actually is inherited from the `Object` class, it is one of the _tagging interfaces_, i.e. interfaces without methods, merely a tag serving as an indicator that the class designer understands the cloning process.

Even if the default shallow copy implemention of `clone` is adequate, `Cloneable` interface still needs implementing and redefining as `public`.

At any rate, cloning is less common than you may think. Less than 5 percent of the classes in the standard library implement `clone`.
