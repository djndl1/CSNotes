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

`char` type may represent one character or just part of a Unicode character. `\u` escape unicode characters. Unicode escape sequences are processed before the code is parsed. In Java, the `char` type describes a _code unit_ in the UTF-16 encoding.

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

Conceptually, Java strings are sequences of Unicode characters. Strings are immutable. 

`.substring()`: extract a substring

- The `String` class gives no methods that let change a character in an existing string.

- `.equals()` tests whether two strings are equal.

`""` is an empty string while a String variable can hold a special value `null`.

`.length()`, `.charAt()` operate by code units. `.codePointCount()`, `codePointAt()` by code points.

String buffers support mutable strings. `StringBuffer` is a thread-safe, mutable sequence of character. It is like a string but can be modified. The principal operations on a StringBuffer` are `append` and `insert`, which are overloaded so as to accept data of any type.


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

A class is the template or blueprint from which objects are made. Encapsulation (information hiding) is a key concept in working with objects. Formally, encapsulation is simply combining data and behavior in one package and hiding the implementation details from users of object. The bits of data in an object are called its _instance fields_, and the procedures that operate on the data are called its _methods_. A specific object that is an instance of a class will have specific values of its instance fields. The set of those values is the current state of the object. Whenever you invoke a method on an object, its state may change.

Objects have three key characteristics:

1. behavior: defined by the methods that is callable

2. state: information about what it currently looks like

3. identity

## Relationships between Classes

- Dependence ("uses-a")

- Aggregation ("has-a")

- Inheritance ("is-a")

## Some issues when defining a class

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

## Class initialization

static blocks: C#'s static constuctor.

```java
static {
    ... //initialization code
}
```

## Object construction

Numbers defaults to 0, `boolean` to `false`, and object references to `null`.

If no constructors are written, a no-argument constructor is provided which sets all the instance fields to default values. Some programmers prefix `a` to every parameters of a method to distinguish from field name. Constructor dispatch is possible through `this` keyword.

```java
public Employee {
    this("Employee #" + nextId, s);
    nextId++;
}
```
Besides explicitly initializing fields, it is possible to use __initialization block__ to initalize variables. This mechanism is never necessary and is not common.

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

In C++, an abstract method is called _pure virtual function_ with a trailing `= 0`. In C++, there is no special keyword to denote abstract classes.

## Access control (TODO: may be expanded)

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

All primitive types have class counterparts: `Integer`, `Long`, `Float`, `Double`, `Short`, `Byte`, `Character` and `Boolean`, inheriting `Number`. Primitive wrappers are all immutable and `final`. It is used when a primitive cannot be used in a generic collection class. Wrapping and unboxing take place automatically between primitive types and wrappers, done by the compiler. Wrappers also have some static related methods.

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

## Records

- immutable type similar to C#'s struct but as a nullable type.

- not meant to replace JavaBeans.

- instance fields of a record are automatically `final`.

```java
record Point(double x, double y) {}
```

- The canonical constructor can have a compact form that show no parameters to preprocess the parameters as a prelude where the parameters are assigned to the fields.

```java
record Range(int from, int to)
{  
    public Range // Compact form
   {      
      if (from > to) // Swap the bounds
      {
         int temp = from;
         from = to;
         to = temp;
      }
   }
}
```

- Custom constructors are allowed but they must call the canoical constructor.


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
    void move(double x, double y);
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

# Lambda Expression

## Syntax

A lambda expression has three ingredients: 

1. a block of code

2. parameters

3. values for the free variables, that is, the variables that are not parameters and not defined inside the code. The _captured_ free variables reference values that don't change. Any captured variable in a lambda expression must be effectively final. 
 
A block of code together with the values of the free variables is a _closure_. The body of a lambda expression has the same scope as a nested block. The same rules for name conflicts and shadowing apply. The `this` keyword refers to the `this` parameter of the method that contains the lambda.


```java
(String first, String second) -> first.length() - second.length()
```

```java
(String first, String second) -> {
    if ((first.length()) < second.length()) return -1;
    else if (first.length() > second.length()) return 1;
    else return 0;
}
```

```java
() -> { for (int i = 100; i >= 0; i--) System.out.println(i); }
```

If the parameter types of a lambda expression can be inferred, then they can be omitted.

```java
Comparator<String> comp = (first, second) -> first.length() == second.length();
```
The result type of a lambda expression is never specified. It is always inferred from context.

it is illegal for a lambda expression to return a value in some branches but not in others.

```java
(int x) -> { if (x >= 0) return 1; }
```

When functional inteface like `ActionListener` with a single `abstract` method is expected, a lambda expression can be supplied. Note that it is possible for an interface to redeclare methods from the `Object` class such as `toString` or `clone` and these declarations do not make the methods abstract. This conversion to interfaces is what makes lambda expressions so compelling. The syntax is short and simple. However, it is almost the only thing that you can do with a lambda expression in Java. You can't even assign a lambda expression to a variable of type `Object`. That is, there is no function object in Java. You have to make lambda expression convert to an object of a functional interface.

The Java API defines a number of very generic functional interfaces in `java.util.function`.

```java
BiFunction<String, String, Integer> comp = (first, second) -> fisrt.length() - second.length();
```

Unfortuately, this `comp` implementing `BiFunction` not `Comparator`, thus not acceptable for `sort` method.

A particularly useful interface in the `java.util.function` package is `Predicate`.

```java
public interface Predicate<T> {
    boolean test(T t);
    // more
}
```


```java
list.removeIf(e -> e == null);
```

Another is `Supplier<T>`. A supplier yields a value of type `T` when called. Suppliers are for _lazy evaluation_.

```java
public interface Supplier<T> {
    T get();
}
```

```java
Localdate hireDay = Objects.requireNonNullElse(day, new LocalDate(1970, 1, 1));
```

is not optimal. Instead,

```java
Localdate hireDay = Objects.requireNonNullElseGet(day, () -> new LocalDate(1970, 1, 1));
```

This method allows creation of the message to be deferred until after the null check is made. However, creating a supplier also cost performance.

## Method reference

It is possible, instead of converting a lambda expression into an object of a functional interface, to pass a named method.

```java
var timer = new Timer(1000, System.out::println);
```

```java
Arrays.sort(strings. String::compareToIgnoreCase);
```

There are three variants of syntax for this.

- `object::instanceMethod`: = `(args) -> object.instanceMethod(args)`;

- `Class::instanceMethod`: = `(first, remainders) -> first.instanceMethod(remainders)`;

- `Class::staticMethod`: = `(args) -> Class::staticMethod(args)`.

The `this` and `super` keywords are also legal to use in method references.

Note that a lambda expression can only be rewritten as a method reference if the body of the lambda expression calls a single method and doesn't do anything else.

It is also possible to pass constructor references, whose method name is `::new`.

```java
Stream<Person> stream = names.stream().map(Person::new);
```

```java
Person[] people = stream.toArray(Person[]::new);
```
## Processing Lambda Expressions

the point of using lambdas is _deferred execution_. There are many reasons for executing code later, such as:

- running the code in a separate thread

- running the code multiple times

- running the code at the right point in an algorithm

- running the code when something happens

- running the code only when necessary

```java
public static void repeat(int n, Runnable action) {
        for (int i = 0; i < n; i++) action.run();
    )
```

There are specializations of generic functional interfaces for primitive types `int`, `long` and `double`. It is more efficient to use these specializations than the  generic interfaces.

```java
public static void repeat(int n, IntConsumer action) {
    for (int i = 0; i < n; i++) action.accept(i);
}
```

`@FunctionalInterface` annotation should be added when defining an interface with a single abstact method.


# Inner Classes

Reasons:

1. Inner classes can be hidden from other classes in the same package.

2. Inner class methods can access the data from the scope in which they are defined, including the data that would otherwise be private.

Inner classes used to be very important for concisely implementing callbacks, which now has been almost replaced by lambda expressions.

In Java, unlike in C++, an object that comes form an inner class has an implicit reference to the outer class object that instantiated it, through which it gains access to the total state of the outer object. The `TimePrinter` has no `beep` field yet it refers to one, which is the field of the `TalkingClock`. An inner class gets to access both its own data and those of the outer object creating it. It eliminates the need to provide unnecessary public methods.


```java
public class TalkingClock
{
   private int interval;
   private boolean beep;
   public TalkingClock(int interval, boolean beep) { . . . }
   public void start() { . . . }
   public class TimePrinter implements ActionListener // an inner class
   {
       public void actionPerformed(ActionEvent event)
       {
            System.out.println("At the tone, the time is "
                + Instant.ofEpochMilli(event.getWhen()));
            if (beep) Toolkit.getDefaultToolkit().beep();
       }
   }
}
```

The compiler mo
difies all inner class constructors, adding a parameter for the outer class reference. The explicit outer class reference is `OuterClass.this`. The `beep` above can be rewritten as `TalkingClock.this.beep`. The inner object constructor can be `outerObject.new InnerClass(args)`. It is also possible to set the outer reference to another object by eplicitly naming it.

```java
var jabberer = new TalkingClock(1000, true);
TalkingClock.TimePrinter listener = jabberer.new TimePrinter();
```

An inner class is referred to as `OuterClass.InnerClass` when it occurs outside the scope of the outer class.

Inner classes are a phenomenon of the compiler, not the virtual machine. Inner classes are translated into regular class files with $ (dollar signs) delimiting outer and inner class names, and the virtual machine does not have any special knowledge about them.

The generated class files of inner classes  have compiler-generated references to outer classes and that of outer classes have accessors, which causes security issues.

## Local Inner Classes: clases locally in a single method

Local classes are never declared with an access specifier (that is, public or private). Their scope is always restricted to the block in which they are declared. Local classes have one great advantage: They are completely hidden from the outside world. Local classes can access effectively final local variables, that is, they never change once assigned. Those variables have `final` fields inside the compiled local inner class.

## Anonymous Inner Classes

In general, the syntax is 

```java
new SuperType(construction parameters)   // SuperType may be an interface or a class
   {
     // inner class methods and data
   }
```

```java
public void start(int interval, boolean beep)
{
   var listener = new ActionListener() 
      {
         public void actionPerformed(ActionEvent event)
         {
            System.out.println("At the tone, the time is "
               + Instant.ofEpochMilli(event.getWhen()));
            if (beep) Toolkit.getDefaultToolkit().beep();
         }
      };
   var timer = new Timer(interval, listener);
   timer.start();
}
```

An anonymous inner class cannot have constructors because the name of a constructor must be the same as the name of a class, and the class has no name. Instead, the construction parameters are given to the superclass constructor. In particular, whenever an inner class implements an interface, it cannot have any construction parameters. Even though an anonymous class cannot have constructors, you can provide an object initialization block.

```java
invite(new ArrayList<String>() {{ add("Harry"); add("Tony"); }});
```


For many years, Java programmers routinely used anonymous inner classes for event listeners and other callbacks. Nowadays, you are better off using a lambda expression.

## Static Inner Classes

If the referece of the outer class is not needed by the inner class, simply placed inside there, the inner class can be declared `static`.

```java
class ArrayAlg {
    public static class Pair {
        // ...
    }

    public static Pair minmax(double[] d) {
        //...
        return new Pair(min, max);
    }
}

ArrayAlg.Pair p = ArrayAlg.minmax();
```

## Proxies (advanced wizardry uncommon in applications)

Proxies are used to create at runtime new classes that implement a given set of interfaces. Proxies are only necessary when you don't yet know at compile time which interfaces to implement. 

TODO

# Exceptions: dealing with errors at runtime

## Dealing with errors

If an operation cannot be completed because of an error, the program ought to 

- return to a safe state and enable the user to execute other commands

or 

- allow the user to save all work and terminate the program gracefully.

The mission of exception handling is to transfer control from where the error occurred to an error handler that can deal with the situation. Errors might be 

- User input errors: typos, syntax errors;

- device errors: hardware not following orders or unavailable;

- physical limitations: full disks, out of memory;

- code errors

The traditional reaction to an error in a method is to return a special error code that the calling method analyzes. Unfortunately, it is not always possible to return an error code. Java allows every method an alternative exit path if it is unable to complete its task in the normal way. An object that encapsulates the error information is thrown. The exception-handling mechanism begins its search for an exception handler that can deal with this particular error condition.

An exception object is always an instance of a class derived from `Throwable`. The `Error` branch of `Throwable` describes internal errors and resource exhaustion situations inside the Java runtime system, where nothing can be done. The `Exception` branch  further splits into `RuntimeException` (a programming error) and `IOException`.

"If it is a `RuntimeException`, it was your fault!" Any exception derived from `Error` or `RuntimeException` is an _unchecked exception_. The compiler checks that an exception handler is provided for all checked exceptions.

In C++, `runtime_error` is equivalent to those exceptions in Java that are not of type `RuntimeException` while `logic_error` is the equivalent of Java's `RuntimeException` and also denotes logical errors in the program.

In Java, a method tells the compiler what can go wrong. A method must declare all the checked exceptions that it might throw. If your method fails to faithfully declare all checked exceptions, the compiler will issue an error message.

```java
public FileInputStream(String name) throws FileNotFoundException // a constructor of FileInputStream
```

Unchecked exceptions are either beyond your control (`Error`) or result from conditions that you should not have allowed in the first place (`RuntimeException`).

If you override a method from a superclass, the checked exceptions that the subclass method declares cannot be more general than those of the superclass method.

```java
String gripe = "Content-length: " + len + ", Received: " + n;
throw new EOFException(gripe);
```

### Catching Exceptions

To catch an exception, set up a `try/catch` block

```java
try {
 code
} catch (ExceptionType e) {
    handler for this type
} catch (ExceptionType2 e) { // if you wanna catch multiple exceptions
    handler for the second type 
} catch (ExceptionType3 | ExceptionType4 e) // you can also hadnle multiple exceptios together, `e` is implicitly final here
```
If any code inside `try` block throws an exception of the class specified in the catch clause, The program skips the remainder of the code in the try block and executes the handler code inside the catch clause. If any of the code in a method throws an exception of a type other than the one named in the catch clause, this method exits immediately.

If we decide that no exception handling should be done, the method should declare the kind of exception it might throw and let the caller handle it. When you propagate an exception, you must add a throws specifier to alert the caller that an exception may be thrown.

If you are writing a method that overrides a superclass method which throws no exceptions, then you must catch each checked exception in your method’s code

As a general rule, you should catch those exceptions that you know how to handle and propagate those that you do not know how to handle.

### Rethrow an exception

You can throw an exception in a catch clause. Typically, you do this when you want to change the exception type.

```java
try
{
   access the database
}
catch (SQLException original)
{
   var e = new ServletException("database error");
   e.initCause(original);
   throw e;
}
```

When the exception is caught, the original exception can be retrieved:

```java
Throwable original = caughtException.getCause();
```

```java
try
{
   access the database
}
catch (Exception e)
{
   logger.log(level, message, e);
   throw e;
}
```

### Cleanup

#### the `finally` clause

The code in the finally clause executes whether or not an exception was caught.

```java
InputStream in = . . .;
try
{
   try
   {
      code that might throw exceptions
   }
   finally
   {
      in.close();
   }
}
catch (IOException e)
{
   show error message
}
```

Errors in the finally clause are reported. 

The body of the finally clause is intended for cleaning up resources. Don’t put statements that change the control flow (return, throw, break, continue) inside a finally clause.

### The `try`-with-resources statement

provided the resource belongs to a class that implements the `AutoCloseable` interface:

```java
void close() throws Exception
```

use `try`-with-resources statement:

```java
try (var in = new Scanner(
      new FileInputStream("/usr/share/dict/words"), StandardCharsets.UTF_8))
{
   while (in.hasNext())
      System.out.println(in.next());
}
```

```java
try (var in = new Scanner(
      new FileInputStream("/usr/share/dict/words"), StandardCharsets.UTF_8);
      var out = new PrintWriter("out.txt", StandardCharsets.UTF_8))
{
   while (in.hasNext())
      out.println(in.next().toUpperCase());
}
```

(Java 9)  you can provide previously declared effectively final variables in the try header:

```java
public static void printAll(String[] lines, PrintWriter out)
{
   try (out) { // effectively final variable
      for (String line : lines)
         out.println(line);
   } // out.close() called here
}
```

If `close` method also throws an exception, the original exception is rethrown and any exceptions thrown by `close()` are considered suppressed. They are automatically caught and added to the original exception with the `addSuppressed` method.

Use the try-with-resources statement whenever you need to close a resource.

### Stack Trace Elements

A _stack trace_ is a listing of all pending method calls at a particular point in the execution of a program. 

The `Throwable` has a `printStackTrace` method. The `StackWalker` class yields a stream of `StackWalker.StackFrame` instances.

```java
StackWalker walker = StackWalker.getInstance();
walker.forEach(frame -> analyze frame)
```

## Tips for Using Exceptions

- Exception handling is not supposed to replace a simple test.

- Do not micromanage exceptions. `try`-block should not be used at a micro-level, i.e. statement-level.

- Make good use of the exception hierarchy. Don’t just throw a `RuntimeException`. Find an appropriate subclass or create your own. Don’t just catch `Throwable`. It makes your code hard to read and maintain. Respect the difference between checked and unchecked exceptions. Do not hesitate to turn an exception into another exception that is more appropriate.

- Do not squelch exceptions.

- When you detect an error, “tough love” works better than indulgence. Do not hesitate to throw an exception.

- Propagating exceptions is not a sign of shame.  Often, it is actually better to propagate the exception instead of catching it.

# Assertions

Assertions are a commonly used idiom of defensive programming. The assertion mechanism allows you to put in checks during testing and to have them automatically removed in the production code.

```java
assert condition
assert condition : expression // expression is passed to the constructor of the `AssertionError` object and tuned into a meesage string
```

Both statements evaluate the condition and throw an `AssertionError` if it is false. 

To enable assertions, running the program with `-enableassertions` or `-ea`. Note that you do not have to recompile your program to enable or disable assertions. Enabling or disabling assertions is a function of the class loader.

You can even turn on assertions in specific classes or in entire packages.

```shell
java -ea:MyClass -ea:com.mycompany.mylib MyApp
```

You can also disable assertions in certain classes and packages with the `-disableassertions` or `-da` option:

Use the `-enablesystemassertions`/`-esa` switch to enable assertions in system classes.

It is also possible to programmatically control the assertion status of class loaders

Assertion failures are intended to be fatal, unrecoverable errors. Assertion checks are turned on only during development and testing. Assertions can be used as a tool for documentation.

# Logging

The logging API is designed to conveniently log program behavior.

- It is easy to suppress all log records or just those below a certain level, and just as easy to turn them back on.

- Suppressed logs are very cheap.

- Log records can directed to different handlers.

- Both loggers and handlers can filter records.

- Log records can be formatted in different ways

- Applications can use multiple loggers

- The logging configuration is controlled by a configuration file.

Third-party alternatives: Log4J2, Logback, SLF4J, Commons Logging.

A logger that is not referenced by any variable can be garbage-collected. To prevent this, save a reference to the logger with a static variable, as in the example above.

Logger names are hierarchical. Logger parents and children share certain properties.

- SEVERE
- WARNING
- INFO, default
- CONFIG
- FINE
- FINER
- FINEST

use `Level.ALL` to turn on logging for all levels or `Level.OFF` to turn all logging off.



# Generic Programming (Not for application development but for library coding)

Generic programming means writing code that can be reused for objects of many different types. Before generic classes were added to Java, generic programming was achieved with polymorphism. The `ArrayList` class simply maintained an array of Object references. Casts are everywhere and there is no error checking. Under the hood, Java generics are nothing more implicit casting, using type erasure (erased to `Object` or the first type they are bound to, which is why in Java a primitive type cannot be used in generics). Type information is not retained at runtime, so it cannot do generic specialization. It's nothing like C++ templates.

(Java 9) It is possible to use diamonds with anonymous subclasses

```java
ArrayList<String> passwords = new ArrayList<>() // diamond OK in Java 9
   {
      public String get(int n) { return super.get(n).replaceAll(".", "*"); }
   };
```

It is common practice to use uppercase letters for type variables, and to keep them short. The Java library uses the variable E for the element type of a collection, K and V for key and value types of a table, and T (and the neighboring letters U and S, if necessary) for "any type at all."

Besides generic classes, it is also possible to define generic methods.

```java

class ArrayAlg
{
   public static <T> T getMiddle(T... a)
   {
      return a[a.length / 2];
   }
}
```

In almost all cases, type inference for generic methods works smoothly.

In Java, it is possible to restrict type variable, like how concepts in C++2a. Note the `extends` keyword, where the one after `extends` may be an interface or a class.

```java
public static <T extends Comparable> T min(T[] a)
{
      if (a == null || a.length == 0) return null;
      T smallest = a[0];
      for (int i = 1; i < a.length; i++)
         if (smallest.compareTo(a[i]) > 0) smallest = a[i];
      return smallest;
}
```

`T` is guaranteed to have a `compareTo` method. Multiple bounding types are connected using `&`.

## Type Erasure

The virtual machine does not have objects of generic types—all objects belong to ordinary classes. Whenever you define a generic type, a corresponding raw type is automatically provided. The name of the raw type is simply the name of the generic type, with the type parameters removed. The type variables are erased and replaced by their bounding types (or `Object` for variables without bounds).

When you program a call to a generic method, the compiler inserts casts when the return type has been erased. Casts are also inserted when you access a generic field.


When you override a method of a generic class, a new bridge method, which has the same signature and return type as the overriden one, is synthesized and it's this method then do some type cast and call the overriding method. Bridge methods are synthesized to preserve polymorphism.

## Restrictions and Limitations

- type parameters cannot be instantiated with primitive types;

- Runtime type inquiry only works with raw types;

```java
if (a instantceof Pair<String>) // Error and isn't of much help
```

- You cannot create arrays of parameterized types

- Varargs warning. You may use `@SafeVarargs` with constructors and methods that are `static`, `fianl` or `private` to suppress the warning.

- You cannot instantiate type variables.

```java
public Pair() { first = new T(); second = new T(); } // illegal
```

The best workaround since Java 8 is to make the caller provide a constructor expression.

```java
public static <T> Pair<T> makePair(Supplier<T> constr)
{
   return new Pair<>(constr.get(), constr.get());
}
```

```java
Pair<String> p = Pair.makePair(String::new);
```

A more traditional workaround is to construct generic objects through reflection, by calling the `Constructor.newInstance` method.

- You cannot construct a generic array

```java
T[] mm = new T[2]; //Error
```

- type variables are not valid in static contexts of generic classes because there can only be one such method.

- You cannot throw or catch instances of a generic class. It is not even legal for a generic class to extend Throwable.

Generic classes can extend or implement other generic classes.

## Wildcard Types

https://docs.oracle.com/javase/tutorial/extra/generics/wildcards.html

http://tutorials.jenkov.com/java-generics/wildcards.html

Since `Pair<Manager>` and `Pair<Employee>` are two unrelated classes, a method accepting `Pair<Employee>` cannot accept `Pair<Manager>`. To solve this problem, we can use wildcard types. In a wildcard type, a type parameter is allowed to vary:

```java
public static void printBuddies(Pair<? extends Employee> p)
{
   Employee first = p.getFirst();
   Employee second = p.getSecond();
   System.out.println(first.getName() + " and " + second.getName() + " are buddies.");
}
```

However, this makes it impossible to call `setFirst` method. `void setFirst(? extends Employee)` refuses to pass any specific type. It is a way of distinguishing between the safe accessor methods and unsafe mutator methods.

Wildcard allows for supertype bound:

```java
public static void minmaxBonus(Manager[] a, Pair<? super Manager> result)
{
   if (a.length == 0) return;
   Manager min = a[0];
   Manager max = a[0];
   
   for (int i = 1; i < a.length; i++)
   {
      if (min.getBonus() > a[i].getBonus()) min = a[i];
      if (max.getBonus() < a[i].getBonus()) max = a[i];
   }
   result.setFirst(min);
   result.setSecond(max);
}
```

Intuitively speaking, wildcards with supertype bounds let you write to a generic object, while wildcards with subtype bounds let you read from a generic object.

With unbound wildcards, the return type of an accessor can only be assigned to an `Object`, a mutator method can never be called.

```java
public static boolean hasNulls(Pair<?> p)
{
    return p.getFirst() == null || p.getSecond() == null;
}
```


## Reflection and Generics

The `Class` class is generic. The type parameter is useful because it allows the methods of `Class<T>` to be more specific about their return types.

TODO
