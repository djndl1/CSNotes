# The Java Collection Framework

## Separating Collection Interfaces and Implementation.

When you use a queue in your program, you don’t need to know which implementation is actually used once the collection has been constructed. Therefore, it makes sense to use the concrete class only when you construct the collection object. With this approach, if you change your mind, you can easily use a different implementation.

## The collection Interface

The fundamental interface for collection classes in the Java library is the Collection interface.

```java
public interface Collection<E>
{
   boolean add(E element);
   Iterator<E> iterator();
   . . .
}
```

## Iterators

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

# Concrete Collections

```bash
                                      +----------+
                                      | Abstract |
                                      |Collection|
                                      +-----^----+
                                            |
        +------------------------------------------------------------------|----------------+
        |                                   |                              |                |
  +-----|-------+                     +-----|-----+                  +-----|-----+          |
  |             |                     |           |                  | Abstract  |          |
  |AbstractList |                     |AbstractSet|                  |   Queue   |          |
  +------|------+                     +-----|-----+                  +-----|-----+          |
         ^                                  ^                              ^                |
         +----------+        +--------------|------------+                 |                |
+--------|------+   |   +----|----+   +----------+  +----|----+    +-------|------+  +------|-----+
|    Abstract   |   |   | HashSet |   | EnumSet  |  | TreeSet |    |PriorityQueue |  | ArrayQueue |
| SequentialList|   |   +----|----+   +----------+  +---------+    +--------------+  +------------+
+--------|------+   |        ^
         ^          |   +----|----+
         |          |   | Linked  |
  +------|-----+    |   | HashSet |
  | LinkedList |    |   +---------+
  +------------+    |
                    |
                    |
               +----|------+
               | ArrayList |
               +-----------+


                                    +---------------+
                                    |               |
                                    |  AbstractMap  |
                                    |               |
                                    +--------|------+
                                             ^
           +----------------|----------------|-----------------|-----------------+
           |                |                |                 |                 |
           |                |                |                 |                 |
      +----|----+    +------|-----+    +-----|-----+     +-----|---+        +----|-----+
      | HashMap |    |   TreeMap  |    |  EnumMap  |     |   Weak  |        | Identity |
      +----|----+    +------------+    +-----------+     | HashMap |        |  HashMap |
           ^                                             +---------+        +----------+
           |
      +----|----+
      | Linked  |
      | HashMap |
      +---------+

```

## `List`

The `List` interface describes an ordered collection in which the position of elements matters.  There are two protocols for visiting the elements: through an iterator and by random access.

- `ArrayList`: An indexed sequence that grows and shrinks dynamically, a dynamically reallocated array of objects. 

- `LinkedList`: An ordered sequence that allows efficient insertion and removal at any location

In Java, all linked lists are doubly linked. `LinkedList.add()` always append an element. There is a position-dependent `.add()` in its iterator (`ListIterator`), which extends `Iterator`, since iterators describe positions in collections. The `ListIterator` interface has methods to tell its position because an iterator keeps a count of its current position.

The collection keeps track of the number of mutating operations (such as adding and removing elements). Each iterator keeps a separate count of the number of mutating operations that it was responsible for. At the beginning of each iterator method, the iterator simply checks whether its own mutation count equals that of the collection. If not, it throws a `ConcurrentModificationException`.

Though not really random access and efficent, `LinkedList` still supplies a `.get()` method, 

```java
for (int i = 0; i < list.size(); i++)
   do something with list.get(i); // totally inefficient
```




- `ArrayDeque`: A double-ended queue that is implemented as a circular array

- `HashSet`: An unordered collection that rejects duplicates

- `TreeSet`: A sorted set

- `EnumSet`: A set of enumerated type values

- `LinkedHashSet`: A set that remembers the order in which elements were inserted

- `PriorityQueue`: A collection that allows efficient removal of the smallest element

- `HashMap`: A data structure that stores key/value associations

- `TreeMap`: A map in which the keys are sorted

- `EnumMap`: A map in which the keys belong to an enumerated type

- `LinkedHashMap`: A map that remembers the order in which entries were added

- `WeakHashMap`: A map with values that can be reclaimed by the garbage collector if they are not used elsewhere

- `IdentityHashMap`: A map with keys that are compared by `==`, not `equals`
