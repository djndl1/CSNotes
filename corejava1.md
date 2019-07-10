# Java intro

Java is a whole platform, with a huge library, containign lots of reusable code, a high-quality executation envrionment that provides services such as security, portability across operating systems, and automatic garbage collection.

The syntax for java is a cleaned-up version of C++ syntax. Java is object-oriented, distributed (having an extensive library for coping with TCP/IP protocols so that accessing remote objects is as easy as accessing a local file system), robust (has a pointer model that eliminates the possiblity of overwriting memory and corrpting data); though not as expected, java security model is still good. Since Java generates bytecode, it is architecture-neutral and its specification has no implementation-dependent aspects. Java's JIT compiler now provides high performance. Java was the first mainstream language to support concurrent programming. 

# Fundamental Programming of Java

The `main` method does not return an exit code to the operating system. To terminate the program with a different exit code, use the `System.exit` method.

`/* */` comments do not nest in Java. You might not be able to deactivate code simply by surrounding it with `/* */`.

## Data types

Java is a strongly typed language, with eight _primitive types_.

`int` is the most pratical. The `byte` and `short` are mainly intended for specialized applications such as low-level file handling. `1_000_000` is legal representation of `1000000`. Java doe not have `unsigned` version of primitive integers.

All floating-point computation follow the IEEE 754 specification. Three flags denoting floating-point values: `Double.POSITIVE_INFINITY`, `Double.NEGATIVE_INIFINITY`, `Double.NaN` are present.

`char` type may represent one character or just part of a Unicode character. `\u` escape unicode characters. Unicode escpae sequences are processed before the code is parsed. In Java, the `char` type describes a _code unit_ in the UTF-16 encoding.

`boolean` cannot be converted into and compared with integers.

## Variables and Constants

`isJavaIdentifierStart` and `isJavaIdentifierPart` methods can be used in the Character class to check what Unicode characters are letters as far as Java is concerned.
  
Declaring multiple variables in a single line is not recommended.

`var` can be used instead of type names if the types of local variables can inferred from the initial value.

```java
var vacationDays = 12
var greeting = "Hello"
var harry = new Employee("Harry Hacker", 50000, 1989, 19, 1); // instead of using `Employee`
```

Use `final` to denote a constant, and `static final` to set up a class constant so that it's available to multiple methods inside a single class.

## Arithmetic Operators

Integer division by 0 raises an exception, whereas floating-point division by 0 yields an infinite or NaN result.

The `Math` class contains an assortment of mathematical functions.

`++` is not recommended inside expressions.

## Strings

Conceptually, Java strings are sequences of Unicode characters.

`.substring()`: extract a substring

- The `String` class gives no methods that let change a character in an existing string.

- `.equals()` tests whether two strings are equal.

`""` is an empty string while a String variable can hold a special value `null`.

`.length()`, `.charAt()` operate by code units. `.codePointCount()`, `codePointAt()` by code points.


## Statements that break control flow

a _labeled break_ statement lets you break out of multiple nested loops.

```java
label:
{
    if (condition) break label;
}
// jumps here when the break statement executes
```

## Array

`type[] var` declaration is preferred. Allelements are initialized after creating an array. It is esssentially the same as a pointer to an array allocated on the heap. In Java, the `[]` operator is predefined to perform _bounds checking_.

Java has a foreach loop

```java
for (variable : collection) statement 
```

`java.util.Arrays` provides many methods to manipulate arrays. static `.Copyof` copy all values of one array into a new array, one of whose common uses is to increase the size of an array.

```java
luckynumbers = Arrays.copyOf(luckyNumbers, 2 * luckyNumbers.length)
```

In the `main` method of a Java program, the name of the program is not stored in the `args` array.


# Objects and Classes

A class is the template or blueprint from which objects are made. Encapsulation (information hiding) is a key concept in working with objects. Formally, encapsulation is simply combining data and behavior in one package and hiding the implementation details from users of object. The bits of data in an object are called its instance fields, and the procedures that operate on the data are called its methods. A specific object that is an instance of a class will have specific values of its instance fields. The set of those values is the current state of the object. Whenever you invoke a method on an object, its state may change.

Objects have three key characteristics:

1. behavior: defined by the methods that is callable

2. state: information about what it currently looks like

3. identity

## Relationships between Classes

- Dependece ("uses-a")

- Aggregation ("has-a")

- Inheritance ("is-a")

## Working with `null` references

When defining a class, it is a good idea to be clear about which fields can be `null`.

```java
if (n == null)
    name = "unknown";
else
    name = n;
```

```java
name = Objects.requireNonNullElse(n, "unknown");
```

or 

```java
Objects.requireNonNull(n, "The name cannot be null");
name = n;
```

If an exception occurs, make the exception report the description of the problem or pinpoints the location of the problem.

Be careful not to let the accessor return a reference to a private mutable field. If a reference to a mutable object must be returned, clone it first.

```java
class Employee {
    ...
    public Date getHireDay() {
        return (Date) hierDay.clone();
    }
}

```

Access priviledges are class-based. A method can access the private data of all objects of its class.

The `final` keywords merely means the object reference will never again refer to a difference object, but the object can be mutated.

Static variables are quite rare, static constants are more common. Static methods don't have a `this` parameter. However, a static method can access a static field. Static factory methods can vary the type of the constructed object. Every class can have a `main` method, a handy trick for unit testing of classes.

## Object construction

Numbers defaults to 0, `boolean` to `false`, and object references to `null`.

If no constructors are written, a no-argument constructor is provided which sets all the instance fields to default values. Some programmers prefix `a` to every parameters of a method to distinguish from field name. Constructor dispatch is possible thorugh `this` keyword.

```java
public Employee {
    this("Employee #" + nextId, s);
    nextId++;
}
```
Besides explicitly initializing fields, it is possible to use __initializtion block__ to initalize variables. This mechanism is never necessary and is not common.

`finalize` method is now deprecated since when the method will be called is undetermined. `Cleaner` class registers an action that is carried out when an object is no longer reacheable. `Runtime.addShutdownHook` adds a "shutdown hook".

## Packages

The main reason for using packages is to guarantee the uniqueness of class names. To absolutely guarantee a unique package name, use an Internet domain name written in reverse.

A class can use all classes from its own package and all _public_ classes from other packages. `import static` permits the importing of static methods and fields, not just class. A source file without `package` is locates in the unnamed package. The compiler does not check the directory structure when it compiles source files.

A JAR file contains multiple class files and subdirectories in a compressed format, saving space and improving performance.

## Inheritance

Factoring out common functionality by moving it to a superclass is routine in OOP. 

A subclass cannot access the private fields of a superclass directly. To access them, use `super.getWhateverYouNeed`. `super` is not a reference to an object but a special keyword that directs the compiler to invoke the superclass method.

Call `super()` to construct the superclass in the constructors of the subclass. Otherwise, the no-argument constructor of the superclass is invoked.

`this` and `super` access a class method/fields or constructors.

In Java, dynamic binding is the default behavior.
