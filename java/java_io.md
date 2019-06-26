# Input

To read console input, first construct a `Scanner` that is attached to `System.in`.

The `Scanner` class is not suitable for reading a password from a consol,e since the input is planly visible to anyone.

```java
Console cons = System.console();
String username = cons.readLine("User name: ");
char[] paswd = cons.readPassword("Password: ");
```

For security reeasons, the password is returns in an array of characters rather a string. After done processing the password, immediately overwrite the array elemetns with a filler value.

## Class `java.util.Scanner`

A simple text scanner which can parse primitive types and strings using regular expressions.


