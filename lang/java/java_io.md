In the Java API, an object form which we can read a sequence of bytes is called an _input stream_. The opposite is an _output stream_. The abstract classes `InputStream` and `OutputStream` are the basis for a hierarchy of I/O classes.

A separate hierarchy provides classes, inheriting from the abstract `Reader` and `Writer` classes for processing Unicode characters.

# Input/Output Stream

The principal methods are `read` and `write`. Both block until the byte is actually read or written. `available()` checks the number of bytes that are currently available. `close()` closes a stream. Closing an output stream also flushes the buffer used for the output stream. Instead of using raw `read` and `write`, use their subclasses.

Java has more than 60 different input/output stream types. For Unicode text, use subclasses of `Reader` and `Writer` classes. The basic methods are similar to those of streams. 

`InputStream`, `OutputStream`, `Reader` and `Writer` all implement the `Closeable` interface. 

All the classes in `java.io` interpret relative path names as starting from the user's working directory. Use `java.io.File.separator` for portable programs.

In Java, some input streams can retrieve bytes from files and other more exotic locations. Other can assemble bytes into more useful data types.

```java
var fin = new FileInputStream("employee.dat);
var din = new DataInputStream(fin);
double x = din.readDouble();
```

These two can be combined using filter stream and additional capabilities are added, such as buffering.

```java
var din = new DataInputStream(
    new BufferedInputStream(
        new FileInputStream("employee.dat")));
```

A `PushbackInputStream` can unread a byte, that is, read a byte to inspect it and then put it back.

```java
var din = new DataInputStream(
   pbin = new PushbackInputStream(
      new BufferedInputStream(
         new FileInputStream("employee.dat"))));
```

The ability to mix and match filter classes to construct useful sequences of input/output stream gives gives an immense amount of flexibility.

```java
var zin = new ZipInputStream(new FileInputStream("employee.zip"));
var din = new DataInputStream(zin);
```

## Text I/O

`OutputStreamWriter` and `InputStreamReader` deal with Unicode code units.

```java
var in = new InputStreamReader(new FileInputStream("data.txt"), StandardCharsets.UTF_8);
```

`PrintWriter` has methods to print strings and numbers in text format. By default, autflushing is not enabled. `System.err` and `System.out` are instances of `PrintStream`. The `PrintStream` class internally converts Unicode characters to the default host encoding in the same way the `PrintWriter` does. Objects of type `PrintStream` act exactly like print writers when you use the `print` and `println` methods, but unlike print writers they allow you to output raw bytes with the `write(int)` and `write(byte[])` methods.

The easiest way to process arbitrary text is the `Scanner` class.

```java
Scanner in = new Scanner(Path.of("myfile.txt"), StandardCharsets.UTF_8);
```

Or 

```java
var content = new String(Files.readAllBytes(path), charset);
List<String> lines = Files.readAllLines(path, charset);

Scanner in = ...;
Stream<String> words = in.tokens;
```

For large files:

```java
// lazy processing
try (Stream<String> lines = Files.lines(path, charset)) {
}
```

To read console input, first construct a `Scanner` that is attached to `System.in`. To read from a file, use a `Scanner` object like 

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

## Class `java.io.Console`

**Methods** to access the character-based console device, if any, associated with the current java virtual machine. 

`System.console()` returns the unique `Console` object associated with the current java VM if any.

Read and write operations are syncronized to guarantee the atomic completion of critical operations, thus related operations may block in multithreaded 

# Output

To write a file, construct a `PrintWriter` object.

```java
PrintWriter out = new PrintWriter("myfile.txt", StandardCharsets.UTF_8);
```

The static `String.format` method creates a formatted string without printing it.


# Binary Data

The `DataOutput` interface provides for converting data from any of the Java primitive types to a series of bytes and writing these bytes to a binary stream.

 In Java, all values are written in the big-endian fashion, regardless of the processor. That makes Java data files platform-independent.
 
Since nobody else uses the Java version of the modification of UTF-8, one should only use the `writeUTF` method to write strings intended for a Java virtual machine.

The `DataInput` interface provides for reading bytes from a binary stream and reconstructing from them data in any of the Java primitive types.

```java
var in = new DataInputStream(new FileInputStream("employee.dat"));
var out = new DataOutputStream(new FileOutputStream("employee.dat"));
```

`RandomAccessFile`, implementing `DataOutput` and `DataInput`, supports both reading and writing to a random access file.

```java
var in = new RandomAccessFile("employee.dat", "r");
var inOut = new RandomAccessFile("employee.dat", "rw");
```

```java
public static void writeFixedString(String s, int size, DataOutput out)
      throws IOException
{
   for (int i = 0; i < size; i++)
   {
      char ch = 0;
      if (i < s.length()) ch = s.charAt(i);
      out.writeChar(ch);
   }
}
```

```java
public static String readFixedString(int size, DataInput in) throws IOException {
    var b = new StringBuilder(size);
    int i = 0;
    var done = false;
    while (!done && i < size) {
      char ch = in.readChar();
      i++;
      if (ch == 0)
        done = true;
      else
        b.append(ch);
    }
    in.skipBytes(2 * (size - i));
    return b.toString();
}  
```

# Working With `Files`

The `Path` interface and `Files` class are much more convenient to use than `File` class. The `Path` interface and `Files` class encapsulate the functionality required to work with the file system on the user’s machine.

## `Path` Interface

A `Path` represents a path that is hierarchical and composed of a sequence of directory and file name elements separated by a special separator or delimiter. A _root component_ identifies a file system hierarchy. The _name element_ is the name of a file or a directory. A `Path` represent a root, a root and a sequence of names, or simply one or more name elements. Accessing a file using an empty path is equivalent to accessing the default directory of the file system.

```java
Path absolute = Paths.get("/home", "harry");
Path relative = Paths.get("myprog", "conf", "user.properties");
```

The `Files` class makes quick work of common file operations. 


```java
byte[] bytes = Files.readAllBytes(path);
List<String> lines = Files.readAllLines(path, charset);
Files.write(path, content.getBytes(charset));
Files.write(path, content.getBytes(charset), StandardOpenOption.APPEND);
Files.write(path, lines);
```

To create files and directories:

```java
Files.createDirectory(path);
Files.createFile(path);
```

To copy, move and delete files:

```java
Files.copy(fromPath, toPath);
Files.move(fromPath, toPath);
Files.delete(path);
```

To get file information:

```java
long fileSize = Files.size(path);
BasicFileAttributes attributes = Files.readAttributes(path, BasicFileAttributes.class);
```

All file systems report a set of basic attributes, encapsulated by the `BasicFileAttributes` interface, which partially overlaps with that information.

To visit directory entries:

```java
try (Stream<Path> entries = Files.list(pathToDirectory))
{
   . . .
}

try (Stream<Path> entries = Files.walk(pathToRoot))
{
   // Contains all descendants, visited in depth-first order
}
```

TODO

# Memory Mapped Files

TODO

# File Locking

A file lock controls access to a file or a range of bytes within a file.
