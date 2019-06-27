# Input

To read console input, first construct a `Scanner` that is attached to `System.in`. To read from a file, use a `Scanner` object like 

```java
Scanner in = new Scanner(Path.of("myfile.txt"), StandardCharsets.UTF_8);
```

The `Scanner` class is not suitable for reading a password from a consol,e since the input is planly visible to anyone.

```java
Console cons = System.console();
String username = cons.readLine("User name: ");
char[] paswd = cons.readPassword("Password: ");
```

For security reeasons, the password is returns in an array of characters rather a string. After done processing the password, immediately overwrite the array elemetns with a filler value.

## Class `java.util.Scanner`

A simple text scanner which can parse primitive types and strings using regular expressions. The scanner can read text from any object with the `Readable` interface. The scanner closes the input source if the input source implements `Closeable` interface when the scanner closes.

The scanner can also use delimters other than whitespaces.

A scanning operation may block waiting for input.

The `next*()` and `hasNext*()` methods skip any input that matches the delimiter pattern and attempt to return the next token.

A scanner is not safe for multithreaded use without external synchronization.

## Class `java.io.Console**

**Methods** to access the character-based console device, if any, associated with the current java virtual machine. 

`System.console()` returns the unique `Console` object associated with the current java VM if any.

Read and write operations are syncronized to guarantee the atomic completion of critical operations, thus related operations may block in multithreaded 

# Output

To write a file, construct a `PrintWriter` object.

```java
PrintWriter out = new PrintWriter("myfile.txt", StandardCharsets.UTF_8);
```

The static `String.format` method creates a formatted string without printing it.


## Class `PrintWriter`

prints formatted representations of objects to a text-output stream. 
