# Java intro

Java is a whole platform, with a huge library, containing lots of reusable code, a high-quality executation envrionment that provides services such as security, portability across operating systems, and automatic garbage collection.

The syntax for java is a cleaned-up version of C++ syntax. Java is object-oriented, distributed (having an extensive library for coping with TCP/IP protocols so that accessing remote objects is as easy as accessing a local file system), robust (has a pointer model that eliminates the possiblity of overwriting memory and corrpting data); though not as great as expected, java security model is still good. Since Java generates bytecode, it is architecture-neutral and its specification has no implementation-dependent aspects. Java's JIT compiler now provides high performance. Java was the first mainstream language to support concurrent programming. 

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

`type[] var` declaration is preferred. All elements are initialized after creating an array. It is esssentially the same as a pointer to an array allocated on the heap. In Java, the `[]` operator is predefined to perform _bounds checking_.

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

If no constructors are written, a no-argument constructor is provided which sets all the instance fields to default values. Some programmers prefix `a` to every parameters of a method to distinguish from field name. Constructor dispatch is possible through `this` keyword.

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

A subclass cannot access the private fields of a superclass directly. To access them, use `super.getWhateverYouNeed()`. `super` is not a reference to an object but a special keyword that directs the compiler to invoke the superclass method.

Call `super()` to construct the superclass in the constructors of the subclass. Otherwise, the no-argument constructor of the superclass is invoked.

`this` and `super` access a class method/fields or constructors.

In Java, dynamic binding is the default behavior.

Attempting to store an `Employee` reference in a `manager` array causes an `ArrayStoreException`.

When calling a method, the compiler first determines the type of the implicit parameter, finds all available methods for that class, then resolves overloading. If the method is _statically bound_, i.e. modified by `private`, `static`, `final` or a constructor, then the method is found. Otherwise the type determined is searched first then its superclass to find the method. The actual search is done through a table lookup instead of searching upwards.

Classes that cannot be extended are called _final_ classes and the `final` modifier in the definition of the class indicates this.

```java

public final class Executive extends Manager {
    ...
}
```

`final` also modifies a specific method in a class. 

The is only one good reason to make a method or class `final`: to make sure its semantics cannot be changed in a subclass.

The JIT compiler does a better job at optimization and inlining a method instead of having programmers figure out whether to `final` a method to avoid the overhead of dynamic binding.

There is only one reason to make a cast - to use an object in its full capacity after its actual type has been temporarily forgotten. It is only possible to cast within an inheritance hierarchy. To ensure a correct cast, use `instanceof` operator before casting (Note that `null` simply returns `false`). However, it may be a design flaw that you need a downcast. In general, it is best to minimize the use of casts and the `instanceof` operator.

## Abstract classes

A class with one or more `abstract` methods must itself be declared abstract. Abstract classes can have concrete methods. Common fields and methods whether abstract or not, should always be moved to the superclass. A class can be declared as `abstract` though it has no abstract methods.

In C++, an abstract method is called _pure virtual function_ with a training `= 0`. In C++, there is no special keyword to denote abstract classes.

## Access control

Any feature declared `private` won't be accessible in other classes: a subclass cannot access the private fields of its superclass. A protected field is accessible by any class in the same package. But a subclass from a different package can only access the protected field of itself, not of any of its superclass.

## `Object`: the cosmic superclass

The `Object` class is the ultimate ancestor of every class in Java. A variable of type `Object` is only useful as a generic holder for arbitrary values. Only the values of _primitive types_ are not objects. All array types are class types.

### `.equals(Object otherObject)`

`.equals()` tests whether one object is considered equal to another. The default, if not overriden, tests whether two objects are the same one. To guard against the possibility that objects may be `null`, use the `Objects.equals()` method.

The Java Language specification requires that the `equals()` method be _reflexive_, _symmetric_, _transitive_, _consistent_ (always returning the same truth value given the same arguments), `x.equals(null)` returns `false`. The symmetry part is kind of subtle, depending on the semantics required.

#### Implementation

1. explicit parameter is named `otherObject`;

2. Test whether `this` happens to be identical to `otherObject`

3. Test whether `otherObject` is `null`

4. Compare the classes of `this` and `otherObject`. If the same semantics holds for all subclasses, use an `istanceof` test, otherwise use `getClass()`;

5. Cast `objectType` to a variable of the calling object's class and compare by the semantics required.

### `.hashCode()` method

Every object has a default hash code derived from the object's memory address. The defintions of `equals` and `hashCode` must be compatible.

### `.toString()` method

The `Object` class defines the `toString` method to print the class name and the hash code of the object. Most `toString` methods have the following format

```java
getClass().getName + "[field1=" + field1 + ",field2=" + field2 + ... + "]"
```

The string concatenation operator `+` implicitly calls `toString` methods. 

`"" + x` is a more general way to call `.toString()`. Howevery, arrays does not call its own `toString` instead, they call `object`'s. To correctly print multidimensional arrays, use `Array.deepToString`.

The `toString` method is a great tool for logging. It is strongly recommended that `toString` be added.

## Object Wrappers and Autoboxing

All primitive types have class counterparts: `integer`, `Long`, `Float`, `Double`, `Short`, `Byte`, `Character` and `Boolean`, inheriting `Number`. Primitive wrappers are all immutable and `final`. It is used when a primitive cannot be used in a generic collection class. Wrapping and unboxing take place automatically between primitive types and wrappers, done by the compiler. Wrappers also have some static related methods.

## Methods with varargs

It is possible to provide methods that can be called with a variable number of parameters.

```java
public static double max(double... values) {
    double largest = Double.NEGATIVE_INFINITY;
    
    for (double v : values) if (v > largest) largest = v;
    return largest;
}
```

## Enumeration Classes

Constructors, methods and fields can be added to an enumerated type:

```java
public enum Size {
    SMALL("S"), MEDIUM("M"), LARGE("L"), EXTRA_LARGE("XL");
    
    private String abbreviation;
    
    private Size(String abbreviation) { this.abbreviation = abbreviation; }
    public String getAbbreviation() { return abbreviation; }
}
```

The constructor of an enumeration is always private. All enumerated types are subclasses of the class `Enum`.

- `valueOf()` - `toString()`


- `.ordinal()`: yields the position of an enumerated constant in the `enum` declaration, counting from zero.

- `.compareTo()`: compare the position enum constants.

## Design Hints for Inheritance

1. Place common operations and fields in the superclass.

2. Do not use `protected` fields if possible. `protected` methods can be useful to indicate methods that are not ready for general use and should be redefined in subclasses.

3. Do not use inheritance unless all inherited methods make sense.

4. Use polymorphism, not type information.

5. Do not overuse reflection. It is not usually appropriate in applications.

# Interfaces

Interfaces are a way of describing what classes should do. It is a set of requirements for the classes that want to conform to the interface. without specifying how they should do it. A class can implement one or more interfaces. Objects of these implementing classes can be used whenever conformance to the interface is required. Typically, the supplier of some service requires that a class conform to a particular interface so that it may perform the service.

While each class can have only one superclass, classes can implement multiple interfaces. This gives the maximum amount of flexibilty in defining a class's behavior. Multiple inheritance makes the language very complex. Few C++ programmers use multiple inheritance.

```java
public interface Comparable<T> {
    int compareTo(T other);
}

class Employee implements Comparable<Employee> {
    public int compareTo(Employee otherObject) {
        return Double.compare(salary, otherObject.salary); // to avoid floating-point overflow instead of using >
    }
}

```

All methods of an interface are automatically `public` (`private` methods are only used as helpers); `static` methods are allowed ; all fields are always `public static final`. The interface itself may be modified by `public` or no acess specifier. Interfaces can provide constants. Interfaces cannot have instance fields. It is possible to supply simple methods in interfaces but they cannot refer to instance fields. Supplying instance fields and methods that operate on them is the job of the classes that implement the interface.

You may not create an object of a certain `interface`, but it is possible to declare such a variable.

```java
Comparable<Employee> x = new Employee();
```

It is possible to extend interfaces to allow for multiple chains of interfaces that go from a greater degree of generality to a greater degree of specialization.

```java
public interface Moveable {
    void move(doube x, double y);
}

public interface Powered extends Moveable {
    double milesPerGallon();
}
```

A default implementation can be supplied for any interface method, tagged with `default`.

```java
public interface Iterator<E> {
    boolean hasNext();
    E next();
    default void remove() {
        throw new UnsupportedOperationException("remove");
    }
}
```

An important use for default methods is _inteface evolution_: a default method is added to the interface, while the class implementing the interface is written before the method is added. 

A subclass may have a inherited method or a default method of an interface with the same name as that of another method of another interface. Concrete methods of a superclass takes priority. Interfaces clash so that the conclicting method must be overriden.

Interfaces can be used in _callback pattern_. Consider a `timer` class in `javax.swing` that calls a method on an object repeatedly between a certain amount of time. The object must implements `ActionListener` interface.

```java
public interface ActionListener {
    void actionPerformed(ActionEvent event);
}

import java.awt.*;
import java.awt.event.*;
import java.time.*;
import javax.swing.*;

public class TimerTest {
  public static void main(String[] args) {
    var listener = new TimerPrinter();

    var timer = new Timer(1000, listener);
    timer.start();

    JOptionPane.showMessageDialog(null, "Quit program?");
    System.exit(0);
  }
}


class TimerPrinter implements ActionListener {
    public void actionPerformed(ActionEvent event) {
        System.out.println("At the tone, the time is " + Instant.ofEpochMilli(event.getWhen()));
        Toolkit.getDefaultToolkit().beep();
    }
}
```
