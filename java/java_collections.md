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








##  Sets



- `TreeSet`: A sorted set, currently implemented as a red-black tree, which requires a `Comparator` on its elements.

- `EnumSet`: A efficient set implementation of enumerated type values, internally implemented as a sequence of bits. A bit is turned on if the corresponding value is present in the set.

- `LinkedHashSet`: A set that remembers the order in which elements were inserted.  As entries are inserted into the table, they are joined in a doubly linked list.

### Hash Sets

In Java, hash tables are implemented as arrays of linked lists (called _buckets_). As of Java 8, the buckets change from linked lists into balanced binary trees when they get full. To find the place of an object in the table, compute its hash code and reduce it modulo the total number of buckets. The resulting number is the index of the bucket that holds the element. The standard library uses bucket counts that are powers of 2 (any value supplied is automatically rounded), with a default of 16.

If the hash table gets too full, it needs to be rehashed. A table with more buckets is created, all elements are inserted into the new table and the original table is discarded. The _load factor_ (default 75% and rehashed to twice the original size) determines when a hash table is rehashed.

- `HashSet`: An unordered collection that rejects duplicates. It is implemented as a set based on a hash table. The hash set iterator visits all buckets in turn.

##  Queues

A priority queue retrieves elements in sorted order after they were inserted in arbitrary order. The priority queue does not sort all its elements. It's implemented as a _heap_, which is a self-organizing binary tree in which the smallest element gravitates to the root. 

A typical use for a priority queue is job scheduling.

- `PriorityQueue`: A collection that allows efficient removal of the smallest element, which must be of a class that implements the `Comparable` interface. The iteration over a `PriorityQueue` does not visit the elements in sorted order.

- `ArrayDeque`: A double-ended queue that is implemented as a circular array

## Map

The Java library supplies two general-purpose implementations for maps: `HashMap` and `TreeMap`.

- `HashMap`: A data structure that stores key/value associations

- `TreeMap`: A map in which the keys are sorted

The collection class library has several map classes for specialized needs.

- `WeakHashMap`: A map whose unused entries can be reclaimed by the GC. It uses _weak references_(???) to hold keys.

- `EnumMap`: A map in which the keys belong to an enumerated type, internally implemented as an array of values. 

- `LinkedHashMap`: A map that remembers the order in which entries were added. As entries are inserted into the table, they are joined in a doubly linked list. A `LinkedHashMap` can also use the so-called _access order_. Every time `get` or `put` is called, the affected entry is moved to the end of the linked list (the hash map order is not affected). Access order is useful for implementing a least-recently-used discipline for a cache.

- `IdentityHashMap`: A map with keys that are compared by `==` and the hash values of its elements are computed using `System.identityHashCode`, through which a hash code is computed from the object's memory address). Different key objects are considered distinct even if they have equal contents.

The collections framework does not consider a map itself as a collection. However, views of the map implements the `Collection` interface or one of its subinterfaces, the set of keys, the collection of values, and the set of key/value pairs.

```java
Set<K> keySet() // an object that implements the `Set` interface
Collection<V> values()
Set<Map, Entry<K, V>> entrySet()
```
