Streams provide a view of data that specifies computations at a higher conceptual level. With a stream, one specifies what he wants to have done, not how to do it.

```java
long count = words.stream().filter(w -> w.length() > 12).count();
long count = words.parallelStream().filter(w -> w.length() > 12).count(); // filtering and couting in parallel 
```

A stream does not store its elements. They may be stored in an underlying collection or generated on demand. Stream operations don't mutate their source. Stream operations are lazy whenever possible.

Any collection can turn into a stream with `.stream()` of the `Collection` method. An array is transformed into a stream using `Stream.of()`.
