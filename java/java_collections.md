# The Java Collection Framework

###### Separating Collection Interfaces and Implementation.

When you use a queue in your program, you don’t need to know which implementation is actually used once the collection has been constructed. Therefore, it makes sense to use the concrete class only when you construct the collection object. With this approach, if you change your mind, you can easily use a different implementation.

###### The collection Interface

The fundamental interface for collection classes in the Java library is the Collection interface.

```java
public interface Collection<E>
{
   boolean add(E element);
   Iterator<E> iterator();
   . . .
}
```

###### Iterators

```java
public interface Iterator<E>
{
   E next();
   boolean hasNext();
   void remove();
   default void forEachRemaining(Consumer<? super E> action);
}
```

By repeatedly calling the `next` method, you can visit the elements from the collection one by one. However, if you reach the end of the collection, the `next` method throws a `NoSuchElementException`. Therefore, you need to call the `hasNext` method before calling next. 

```java
Collection<String> c = . . .;
Iterator<String> iter = c.iterator();
while (iter.hasNext())
{
   String element = iter.next();
   do something with element
}
```

The `foreach` loop is just a syntactic sugar of the above loop. The “for each” loop works with any object that implements the `Iterable` interface. Instead of writing a loop, you can call the `forEachRemaining` method with a lambda expression that consumes an element. The lambda expression is invoked with each element of the iterator, until there are none left.

Java iterators are more like Python iterators, unlike iterators in C++ STL, which are modelled on indices and pointers.

The `remove` method of the Iterator interface removes the element that was returned by the last call to `next`.

```java
Iterator<String> it = c.iterator();
it.next(); // skip over the first element
it.remove(); // now remove it
```

###### Generic Utility Methods

The `Collection` interface declares quite a few useful methods that all implementing classes must supply:

```java
int size()
boolean isEmpty()
boolean contains(Object obj)
boolean containsAll(Collection<?> c)
boolean equals(Object other)
boolean addAll(Collection<? extends E> from)
boolean remove(Object obj)
boolean removeAll(Collection<?> c)
void clear()
boolean retainAll(Collection<?> c)
Object[] toArray()
<T> T[] toArray(T[] arrayToFill)
```

The library supplies a class `AbstractCollection` that leaves the fundamental methods size and iterator abstract but implements the routine methods in terms of them.
