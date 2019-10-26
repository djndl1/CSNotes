Streams provide a view of data that specifies computations at a higher conceptual level. With a stream, one specifies what he wants to have done, not how to do it.

```java
long count = words.stream().filter(w -> w.length() > 12).count();
long count = words.parallelStream().filter(w -> w.length() > 12).count(); // filtering and couting in parallel 
```

A stream does not store its elements. They may be stored in an underlying collection or generated on demand. Stream operations don't mutate their source. Stream operations are lazy whenever possible.

# Creation

Any collection can turn into a stream with `.stream()` of the `Collection` method. An array is transformed into a stream using `Stream.of()` (and `Arrays.streaṁ()` from an array).

```java
Stream<String> song = Stream.of("gently", "down", "the", "stream");
Stream<String> silence = Stream.empty();
```

To make infinite streams:

```java
Stream<String> echos = Stream.generate(() -> "Echo");
Stream<Double> randoms = Stream.generate(Math::random);
Stream<BigInteger> integers = Stream.iterate(BigIntegers.ZERO, n -> n.add(BigInteger.ONE));
```

A finite stream is produced by adding a stop condition:

```java
var limit = new BigInteger("10000000");
Stream<BigInteger> integers
   = Stream.iterate(BigInteger.ZERO,
      n -> n.compareTo(limit) < 0,
      n -> n.add(BigInteger.ONE));
```

`Stream.ofNullable()` makes a short stream from an object, either of `null`  or length 1 otherwise.

# `filter`, `map`, `flatmap`

A stream transformation produces a stream whose elements are derived from those of another stream.

```java
Stream<String> lowercaseWords = words.stream().map(String::toLowerCase);
Stream<String> firstLetters = words.stream().map(s -> s.substring(0, 1));
```

```java
public static Stream<String> codePoints(String s)
{
   var result = new ArrayList<String>();
   int i = 0;
   while (i < s.length())
   {
      int j = s.offsetByCodePoints(i, 1);
      result.add(s.substring(i, j));
      i = j;
   }
   return result.stream();
}

Stream<Stream<String>> result = words.stream().map(w -> codePoints(w));
Stream<String> flatResult = words.stream().flatMap(w -> codePoints(w));
```

# Extracting Substreams and Combining Streams

`stream.limit(n)` returns a new stream that ends after `n` elements. `stream.skip(n)` does the exact opposite:

```java
Stream<Double> randoms = Stream.generate(Math::random).limit(100);
Stream<String> words = Stream.of(contents.split("\\PL+")).skip(1);
```

`stream.takeWhile(pred)` takes all elements from the stream while the predicate is true and then stops. The `dropWhile` method does the opposite

```java
Stream<String> initialDigits = codePoints(str).takeWhile(
   s -> "0123456789".contains(s));
   
Stream<String> withoutInitialWhiteSpace = codePoints(str).dropWhile(
   s -> s.trim().length() == 0);
```

Two streams are concatenated with `Stream.concat()` (the first cannot be infinite):

```java
Stream<String> combined = Stream.concat(
   codePoints("Hello"), codePoints("World"));
   // Yields the stream ["H", "e", "l", "l", "o", "W", "o", "r", "l", "d"]
```

`distinct` returns a stream that remove duplicates of the original stream, where the duplicates need not to be adjacent.

```java
Stream<String> uniqueWords
   = Stream.of("merrily", "merrily", "merrily", "gently").distinct();
```

`sorted` sorts a stream:

```java
Stream<String> longestFirst
   = words.stream().sorted(Comparator.comparing(String::length).reversed());
```

`peek` yields another stream with the same elements as the original, with a function invoked every time an element is retrieved.

```java
Object[] powers = Stream.iterate(1.0, p -> p * 2)
   .peek(e -> System.out.println("Fetching " + e))
   .limit(20).toArray();
```
