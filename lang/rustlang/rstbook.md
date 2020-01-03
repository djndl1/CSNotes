# Project Management with `cargo`; Packages, Crates and Modules

`Cargo.lock` ensures reproducible builds by recording versions of the dependencies when first building, instead of resolving dependencies whenever building. `cargo update` bypasses this.

## Module System

### Packages and Crates

- _crate_: a binary or library; 

- _package_: one or more crates that provide a set of functionality. A package contains a `Cargo.toml` file that describes how to build those crates. `Cargo` follows a convention that `src/main.rs` is the crate root of a binary crate with the same name as the package. `src/lib.rs`, if it exists, then the package contains a library crate with the same name as the package, and `src/lib.rs` is its crate root. A package can have multiple binary crates by placing files in the `src/bin` directory.

- _module_: organizes code within a crate into groups for readability and easy reuse. Modules also control the privacy of items, which is whether an item can be used by outside code (public) or is an internal implementation detail and not available for outside use (private). Modules also define Rust's privacy boundary. All items (functions, methods, structs, enums, modules, and constants) are private by default. Items in a parent module can’t use the private items inside child modules, but items in child modules can use the items in their ancestor modules. To make an item public, both the module and the item needs to be marked `pub`. Also, fields of a struct in a module are private by default. All variants of a public enum are public.

```rust
mod front_of_house {
    mod hosting {
        fn add_to_waitlist() {}

        fn seat_at_table() {}
    }

    mod serving {
        fn take_order() {}

        fn serve_order() {}

        fn take_payment() {}
    }
}
```

```
crate
 └── front_of_house
     ├── hosting
     │   ├── add_to_waitlist
     │   └── seat_at_table
     └── serving
         ├── take_order
         ├── serve_order
         └── take_payment
```

A module can be split into different files, with a file structure similar to Java, not forced though. Using a semicolon after `mod` clause rather than using a block tells Rust to load the contents of the module from another file with the same name as the module. 

```rust
// src/lib.rs
mod front_of_house; // declare the module

pub use crate::front_of_house::hosting;

pub fn eat_at_restaurant() {
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
}
```

```rust
// src/front_of_house.rs

pub mod hosting; // declared its submodule
```

```rust
// src/front_of_house/hosting.rs
pub fn add_to_waitlist() {}
```

- _path_: absolute path starts from a crate root by using a crate name or a literal `crate` (like `/`); relative path starts from the current module and uses `self` (like `.`), `super` (like `..`), or an identifier in the current module.

- `use` keyword: brings paths into scope. It is idiomatic to use a full path to bring in structs, enums and other items with `use` unless there are name conflicts. For a function, however, it is common to `use` its surrounding module rather than bringing in itself directly. When we bring a name into scope with the `use` keyword, the name available in the new scope is private. To enable the code that calls our code to refer to that name as if it had been defined in that code’s scope, we can combine `pub` and `use`. This technique is called _re-exporting_. Re-exporting is useful when the internal structure of the  code is different from how programmers calling your code would think about the domain. Doing so makes our library well organized for programmers working on the library and programmers calling the library. To `use` multiple items under the same prefix,

```rust
use std::{cmp::Ordering, io, self}; // self adds `std` itself
```

- Glob operator: bring all public items defined in a path into scope:

```rust
use std::collections::*;
```

- `as` keyword: provides an alias

```rust
use std::fmt::Result;
use std::io::Result as IoResult;
```


# Guessing game

```rust
use std::io; // bring in `std::io` library
use rand::Rng;
use std::cmp::Ordering;

fn main() {
    println!("Guess the number!");  // a macro

    let secret_number = rand::thread_rng().gen_range(1, 101);

    //println!("The secrete number is: {}", secret_number);

    // make an infinite loop
    loop {
        println!("Please input your guess.");

        // `let` creates a variable. `mut` makes it mutable.
        // `new` an associated function (static method) of `String` type
        let mut guess = String::new();

        // `&` indicates a reference
        io::stdin().read_line(&mut guess)
            .expect("Failed to read line");
        // `read_line` returns an `io::Result` type, an enumeration type (`Ok` or `Err`).
        // An error handling mechanism

        // convert the string to a u32
        let guess: u32 = match guess.trim().parse() { // `: type` annotates this as `u32`
            Ok(num) => num,
            Err(_) => continue,     // `_` a catchall value
        };

        println!("You guessed: {}", guess);

        // returns a variant of the `Ordering` enum
        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"), // an arm
            Ordering::Greater => println!("Too big"), // pattern
            Ordering::Equal => {
                println!("You win!");
                break; // break out the loop
            }
        }
    }
}

```


# Basic Concepts

## Mutability

To ensure safety and easy concurrency, by default variables are immutable. `mut` makes a variable mutable. Constants are declared using `const` and its type must be annotated.

```rust
const MAX_POINTS: u32 = 100_000;
```

A new variable with the same name as a previous variable shadows the previous one.

The compiler can usually infer what type we want to use based on the value and how we use it. In cases when many types are possible, we must add a type annotation:

## Data Types

```rust
let guess: u32 = "42".parse().expect("Not a number");
```

### Scalar Types

- integer: `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `i128`, `u128`, `isize`, `usize` (word-length); Octal is denoted by `0o`, binary `0b`, byte `b'A'`. If overflow occurs, Rust performs two's complement wrapping.

- floating-point: `f32`, `f64` (default), conforming to IEEE 754

- booleans: `bool`. Rust will not automatically try to convert non-Boolean types to a Boolean.

- characters: `char`, single quote, four bytes Unicode

### Compound Types


- `tuple`: a general way of grouping together some number of other values with a variety of types

```rust
let tup: (i32, f64, u8) = (500, 6.4, 1);
let (x, y, z) = tup; // destructuring
let five_hundred = x.0;
let six_point_four = x.1;
let one = x.2;
```

- `array`: every element of an array must have the same type; allocated on the stack

```rust
let a = [1, 2, 3, 4, 5];
let b: [i32; 5] = [1, 2, 3, 4, 5];
let a = [3; 5]; // an array of five 3s

let first = a[0];
let second = a[1];
```

Rust does array boundary check.

## Control Flow

- `if`: repeated `else if` can be replaced by `match`. Since `if` is an expression:

```rust
let number = if condition {
    5
} else {
    6
};
```

Blocks of code evaluate to the last expression in them.

- `loop`: infinite loop; a loop can return a value:

```rust
fn main() {
    let mut counter = 0;

    let result = loop {
        counter += 1;

        if counter == 10 {
            break counter * 2;
        }
    };

    println!("The result is {}", result);
}
```

- `while`

- `for`: iterate over a collection:

```rust
    let a = [10, 20, 30, 40, 50];

    for element in a.iter() {
        println!("the value is: {}", element);
    }
```

## Functions

Function bodies are made up of a series of statements optionally ending in an expression. Function definitions, variable creations are statements. Calling a function/macro is an expression, blocks of code are expressions.

```rust
let y = {
    let x = 3;
    x + 1       // expressions do not include ending semicolons
} // evalutes to 4
```

```rust
fn func_name(param1: param_type1, param2: param_type2) -> return_type {
    // function body
}
```

```rust
fn plus_one(x: i32) -> i32 {
    x + 1
}
```

# Ownership

__Ownership__ is the central feature of Rust. Memory is managed through a system of ownership with a set of rules that the compiler checks at compile time.

- Each value has a variable called its _owner_;

- There can only be one owner at a time;

- When the owner goes out of scope, the value will be dropped.

Assignment in Rust by default moves a value (Stack-only data are copied). Rust will never automatically create deep copies of data. If deep copy is needed, use a common method `clone`.

Rust has a special annotation `Copy` trait that indicates that a type is placed on stack only. If a type has the `Copy` trait, an older variable is still usable after assignment. All basic numeral types, `bool`, `char`, tuples of `Copy` types are of `Copy` trait.

Passing a variable to a function will move or copy. Returning values can also transfer ownership, which follows the same pattern.

References allow for referring to some value without taking ownership of it. `&var` creates a reference that refers to `var`. The value will not be dropped when the reference goes out of scope. The signature of the function uses `&` to indicate the type of the parameter is a reference. Having references as function parameters is called _borrowing_. References are immutable by default.

```rust
fn calculate_length(s: &String) -> usize {
    s.len()
}

fn change_string(s: &mut String) {
    s.push_str(" World!");
}

```

However, you can have only one mutable reference to a particular piece of data in a particular scope. This can prevent data races. Also, we cannot have a mutable reference while we have an immutable one. A reference's scope starts from where it is introduced and continues through the last time that reference is used (not just block scope).

```rust
// totally legal
fn main() {
    let mut s = String::from("hello");

    let r1 = &s; // no problem
    let r2 = &s; // no problem
    println!("{} and {}", r1, r2);
    // r1 and r2 are no longer used after this point

    let r3 = &mut s; // no problem
    println!("{}", r3);
}
```

```rust
fn main() {
    let mut s = String::from("hello");

    let r1 = &s; // no problem
    let r2 = &s; // no problem
    
    println!("{} and {}", r1, r2);
    // r1 and r2 are no longer used after this point

    let r3 = &mut s; // no problem
    // s.push_str("abc");  E0499
    r3.push_str("efg");
    println!("{}", r3);
    s.push_str("abc"); // okay
}
```

In Rust, the compiler guarantees that references will never be dangling references. The compiler will ensure that the data a reference points to will not go out of scope before the reference to the data does.

```rust
fn no_dangle() -> String {
    let s = String::from("Dangling");
    s
}


fn dangles() -> &String {
    let s = String::from("Dangling");
    &s
}
```

- A _string slice_  is a reference to part of a `String`:

```rust
let mut s = String::from("hello world");

    let hello = &s[0..5];
    let world = &s[6..11];

    let len = s.len();
    let slice = &s[3..len];
    let slice2 = &s[3..];
    let slice3 = &s[0..len];
    let slice4 = &s[..];
    
    //  String slice range indices must occur at valid UTF-8 character boundaries. 
```

```rust
fn first_word(s: &String) -> &str{
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item = b' ' {
            return &s[0..i];
        }
    }
    &s[..]
}

fn main() {
    let mut s = String::from("hello world");
    let word = first_word(&s);
    s.clear();      // 
    println!("the first word is: {}", word);
}
```

```bash
error[E0502]: cannot borrow `s` as mutable because it is also borrowed as immutable
 --> rust_scratchpad.rs:4:5
  |
3 |     let word = first_word(&s);
  |                           -- immutable borrow occurs here
4 |     s.clear();
  |     ^^^^^^^^^ mutable borrow occurs here
5 |     println!("the first word is: {}", word);
  |                                       ---- immutable borrow later used here
```

String literals are slices. `&str` makes a string slice used as a parameter.

# Structs

## Definition and Instantiation

```rust
struct  User {
    // fields
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

fn main() {
    let mut user1 = User {
        email: String::from("someone@email.com"),
        username: String::from("someusername"),
        sign_in_count: 1,
        active: true
    };
    user1.email = String::from("someother@email.com");

    let user2 = build_user(String::from("another"), String::from("another@email.com"));
    let user3 = User {
        ..user2
    };
    let user4 = User {
        username: String::from("user4"),
        ..user3
    };
}

fn build_user(email: String, username: String) -> User {
    User {
        username,
        email,
        sign_in_count: 1,
        active: true,
    }
}
```

Structs can be defined in a similar way to tuples, called _tuple structs_.

```rust
struct Pixel(u8, u8, u8);
    let blackpoint = Pixel(0, 0, 0);
    let Pixel(red, green, blue) = blackpoint; // destructuring
```

- Unit-like struct: structs without any data, used to implement a trait.

It’s possible for structs to store references to data owned by something else, but to do so requires the use of _lifetimes_.

- `Debug` trait output:

```rust
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

fn area(rectangle: &Rectangle) -> u32{
    rectangle.height * rectangle.width
}

fn main() {
    let rect1 = Rectangle {width: 30, height: 50};
    println!("{:#?}", rect1);
}
```

```bash
Rectangle {
    width: 30,
    height: 50,
}
```

```rust
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32{
        self.height * self.width
    }

    fn can_hold(&self ,other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.width
    }

    fn square(size: u32) -> Rectangle{
        Rectangle {width: size, height: size}
    }
}
```

It is possbile to separate these methods into multiple `impl` blocks.

# Enums and Pattern Matching

```rust
enum IpAddr {
    V4(String),
    V6(String),
}

let home = IpAddr::V4(String::from("127.0.0.1"));

let loopback = IpAddr::V6(String::from("::1"));
```

Each variant can have different types and amount of associated data.

```rust
enum IpAddr {
    V4(u8, u8, u8, u8),
    V6(String),
}
```


```rust
enum Message {
    Quit,
    Move { x: i32, y: i32 }, // anonymous struct
    Write(String),
    ChangeColor(i32, i32, i32), // three i32
}
```

Enums can have their methods:

```rust
impl Message {
    fn call(&self) {
        // method body would be defined here
    }
}
```

Expressing a concept in terms of the type system means the compiler can check whether a case is handled the way it should be. Rust doesn’t have the null feature that many other languages have. Rust does not have nulls, but it does have an enum that can encode the concept of a value being present or absent. This enum is `Option<T>`.  Everywhere that a value has a type that isn’t an `Option<T>`, you can safely assume that the value isn’t null.

## `match` Control Flow Operator

Patterns can be made up of literal values, variable names, wildcards and many other things.

```rust
#[derive(Debug)] // so we can inspect the state in a minute
enum UsState {
    Alabama,
    Alaska,
    // --snip--
}

enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState),
}

fn value_in_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter(state) => {
            println!("State quarter from {:?}!", state);
            25
        },
    }
}
```

With `Option<T>`: 

```rust
fn plus_one(x: Option<i32>) -> Option<i32> {
    match x {
        None => None,
        Some(i) => Some(i + 1),
    }
}

let five = Some(5);
let six = plus_one(five);

let none = plus_one(None);
```

Rust also has a pattern `_` we can use when we don’t want to list all possible values and `()` (unit value) when doing nothing.

```rust
let some_u8_value = 0u8;
match some_u8_value {
    1 => println!("one"),
    3 => println!("three"),
    5 => println!("five"),
    7 => println!("seven"),
    _ => (),
}
```

## `if let`

The `if let` syntax combines `if` and `let` into a less verbose way to handle values that match one pattern while ignoring the rest.

```rust
if let Some(3) = some_u8_value { // not an assignment
    println!("three);
}
```

```rust
let mut count = 0;
if let Coin::Quarter(state) = coin {
    println!("State quarter from {:?}!", state);
} else {
    count += 1;
}
```

is equivalent to

```rust
let mut count = 0;
match coin {
    Coin::Quarter(state) => println!("State quarter from {:?}!", state),
    _ => count += 1,
}
```

# Common Collections

The data that collections point to is stored on the heap.

```rust
let mut v = Vec::new();
    v.push(12.0);

    let mut v2 = vec![1, 2, 3];
    v2.push(5);
    v2.push(6);

    let v3 = vec![1, 2, 3, 4, 5];
    let v4 = vec![1; 5];

    let third: &i32 = &v[2]; // unsafe
    println!("{}", third);

    match v3.get(2) {   // safe access, return None if not available
        Some(third) => println!("The third element is {}", third);
        None => println!("There is no third element.", );
    }
    
    
```

With `enum`, we can store multiple types in a vector:

```rust
enum SpreadsheetCell {
    Int(i32),
    Float(f64),
    Text(String)
}

fn main() {
    let row = vec![
        SpreadsheetCell::Int(3),
        SpreadsheetCell::Text(String::from("blue")),
        SpreadsheetCell::Float(10.12),
    ];
}
```

Rust needs to know what types will be in the vector at compile time so it knows exactly how much memory on the heap will be needed to store each element.

## `String`

`String` is growable, mutable, owned, UTF-8 encoded string type. 

`push_str()` does not take ownership of another string. `+` concatenation is hard to use and has an ugly syntax.

Rust strings don't support indexing. Internally, `String` is a wrapper over a `Vec<u8>`. Using string slicings may not be safe, either. The best way to perform operations on individual Unicode scalar values is to use `s.chars()` method.

```rust
    let mut s = String::from("foo");
    s.push_str("bar");

    let s1 = String::from("Hello, ");
    let s2 = String::from("world!");
    let s3 = s1 + &s2; // s1 is moved and no longer available
    println!("{}", s3);


    // it's better to use `format!`
    let s4 = String::from("tic");
    let s5 = String::from("tac");
    let s6 = String::from("toe");
    let s7 = format!("{}-{}-{}", s4, s5, s6);
    println!("{}", s7);

    for c in s7.chars() {
        println!("{}", c);
    }
    for b in s7.bytes() {
        println!("{}", b);
    }
```

## `HashMap`

`insert` an owned value into a `HashMap` moves the data. However, we can move references, but the references must be valid at least as long as the hash map is valid. By default, HashMap uses a “cryptographically strong” hashing function that can provide resistance to Denial of Service (DoS) attacks. 
```rust
    use std::collections::HashMap;

    let mut scores = HashMap::new();
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    let teams = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 50];

    let scores2: HashMap<_, _> = teams.iter().zip(initial_scores.iter()).collect();


    let s = String::from("Moved");
    scores.insert(s, 30);
    // println!("{}", s);
    //                ^ value borrowed here after move
    let team_name = String::from("Blue");
    let score = scores.get(&team_name); // returns an Option<&T>

    scores.entry(String::from("Red")).or_insert(20);
    for (name, scor) in &scores {
        println!("{}: {}", name, scor);
    }

    let text = "hello world wonderful day";
    let mut counts = HashMap::new();
    for word in text.split_whitespace() {
        let count = counts.entry(word).or_insert(0);
        *count += 1;
    }

    println!("{:?}", counts);
```

# Error Handling

Rust groups errors into two major categories: recoverable and unrecoverable errors. Rust doesn’t have exceptions. Instead, it has the type `Result<T, E>` for recoverable errors and the `panic!` macro that stops execution when the program encounters an unrecoverable error. 

By default, when a panic occurs, the program starts unwinding, which means Rust walks back up the stack and cleans up the data from each function it encounters.

```rust
enum Result<T, E> {
    Ok(T),   // the type of the value that will be returned in a success case within the `Ok` variatn
    Err(E),  // 
}
```

```rust
use std::fs::File;

fn main() {
    let f = match File::open("rust_scratchpad.rs") {
        Ok(file) => file,
        Err(error) => match error.kind() {
            ErrorKind::NotFound => match File::create("hello.txt") {
                Ok(fc) => fc,
                Err(e) => panic!("Problem creating thefile {:?}", e),
            },
            other_error => panic!("Problem opening the file: {:?}", other_error),
        },
    };
}
```

- `Result<T, E>.unwrap()`: a shortcut that is implemented like the `match` expression above

- `Result<T, E>.expect()`: the `panic!` error message can be customized.

Error propagating is done by returning `Result<T, E>`.

```rust
// full version
use std::io;
use std::io::Read;
use std::fs::File;

fn read_username_from_file() -> Result<String, io::Error> {
    let f = File::open("hello.txt");

    let mut f = match f {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut s = String::new();

    match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e),
    }
}

// short version using the `?` operator
use std::io;
use std::io::Read;
use std::fs::File;

fn read_username_from_file() -> Result<String, io::Error> {
    let mut f = File::open("hello.txt")?; 
    // { ok(file) => file,
    //  Err(e) => return Err(e),
    // } 
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}

// even shorter
use std::fs::File;
use std::io;
use std::io::Read;

fn main() {
    let mut s = String::new();
    File::open("hello.txt")?.read_to_string(&mut s)?;
    Ok(s)
}

// shorter!
fn read_username_from_file() -> Result<String, io::Error> {
    fs::read_to_string("hello.txt")
}
```

Error values that have the `?` operator called on them go through the `from` function. The error type received is converted into the error type defined in the return type of the current function. The `?` operator can only be used in functions that have a return type of `Result<T, E>`. One valid return type for `main` is `()`, and another valid type is `Result<T, E>`.

Returning Result is a good default choice when you’re defining a function that might fail. Call `unwrap` or `expect` during prototyping and testing. Even if sometimes it's impossible to fail in a certain situation, the compiler will insists that the error be handled (it doesn't have enough information). The solution is to add a `unwrap` to ensure this.

It’s advisable to have your code panic when it’s possible that your code could end up in a bad state. 

TODO

# Generics, Traits and Lifetimes

## Generic Data Type

```rust
fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];

    for item in list.iter() {
        if item > largest {
            largest = item;
        }
    }
    largest
}

fn largest<T: PartialOrd + Copy>(list: &[T]) -> T {
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }
    largest
}

struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }
}
```

Specialization is possible:

```rust
impl Point<f32> {
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}
```

Generic type parameters in a struct definition aren’t always the same as those used in that struct’s method signatures. Some generic parameters are declared with impl and some are declared with the method definition.

```rust
struct Point<T, U> {
    x: T,
    y: U,
}

impl<T, U> Point<T, U> {
    fn mixup<V, W>(self, other: Point<V, W>) -> Point<T, W> {
        Point {
            x: self.x,
            y: other.y,
        }
    }
}

    let p1 = Point { x: 5, y: 10.4 };
    let p2 = Point { x: "Hello", y: 'c'};

    let p3 = p1.mixup(p2);
```

Rust accomplishes this by performing monomorphization of the code that is using generics at compile time. Monomorphization is the process of turning generic code into specific code by filling in the concrete types that are used when compiled. When the code runs, it performs just as it would if we had duplicated each definition by hand.

## Traits

A trait tells the compiler about functionality a particular type has and can share with other types. Traits define shared behavior in an abstract way. Traits are similar to a feature often called _interfaces_ in other languages. A type’s behavior consists of the methods we can call on that type. Trait definitions are a way to group method signatures together to define a set of behaviors necessary to accomplish some purpose.

```rust
pub trait Summary {
    fn summarize(&self) -> String {  // a default implementation
        format!("(Read more from {}...)", self.summarize_author())
    }

    fn summarize_author(&self) -> String;
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{} by {} ({})", self.headline, self.author, self.location)
    }

    fn summarize_author(&self) -> String {
        format!("{}", self.author)
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for Tweet {
    // fn summarize(&self) -> String 
    //     format!("{}: {}", self.username, self.content)
    // }

    fn summarize_author(&self) -> String {
        format!("@{}", self.username)
    }
}
```

It's impossible to implement an external trait for an external type. This ensures other people's code can't break one's code and vice versa.

Default implementations can call other methods in the same trait, even if those other methods don’t have a default implementation. 

```rust
pub fn notify(item: impl Summary) {
    print!("Breaking news! {}", item.summarize());
}

// just a syntax sugar for trait bound syntax

pub fn notify<T: summary>>(item: T) {
    print!("Breaking news! {}", item.summarize());
}

// an alternate form with `where` clause, like C++ concept
fn some_function<T, U>(t: T, u: U) -> i32
where T: Display + Clone,
      U: Clone + Debug
{
//...
}

fn return_summarizable() -> impl Summary {       // however, this `impl Summary` is only one type that implements `Summary`
    Tweet {
        username: String::from("horse_ebooks"),
        content: String::from("of course as ou probably already know, people"),
        reply: false,
        retweet: false,
    }
}
```

Traits bounds can be used to conditionally implement methods:

```rust
use std::fmt::Display;

struct Pair<T> {
    x: T,
    y: T,
}

impl<T> Pair<T> {
    fn new(x: T, y: T) -> Self {
        Self {
            x,
            y,
        }
    }
}

impl<T: Display + PartialOrd> Pair<T> {
    fn cmp_display(&self) {
        if self.x >= self.y {
            println!("The largest member is x = {}", self.x);
        } else {
            println!("The largest member is y = {}", self.y);
        }
    }
}
```

## Lifetimes

We must annotate lifetimes when the lifetimes of references could be related in a few different ways. The main aim of lifetimes is to prevent dangling references. The Rust compiler has a _borrow checker_ that compares scopes to determine whether all borrows are valid.

Lifetime annotations don’t change how long any of the references live. One lifetime annotation by itself doesn’t have much meaning. Lifetime annotations describe the relationships of the lifetimes of multiple references to each other without affecting the lifetimes.

```rust
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
```

The function signature also tells Rust that the string slice returned from the function will live at least as long as lifetime `'a`. In practice, it means that the lifetime of the reference returned by the `longest` function is the same as the smaller of the lifetimes of the references passed in. The borrow checker should reject any values that don't adhere to these constraints. When we pass concrete references to `longest`, the concrete lifetime that is substituted for `'a` is the part of the scope of `x` that overlaps with the scope of `y`.

```rust
fn main() {
    let string1 = String::from("long string is long");
    let result;
    {
        let string2 = String::from("xyz");
        result = longest(string1.as_str(), string2.as_str());
    }
    println!("The longest string is {}", result);
}
```

Had we not annotated `longest`, the compiler could not have known that `result` had a lifetime as long as `string2`. It might well assume that `result`'s lifetime is the outer scope.

It's possible for structs to hold references

```rust
struct ImportantExcerpt<'a> {
    part: &'a str,
}

fn main() {
    let novel = String::from("Call me Ishmael. Some years ago...");
    let first_sentence = novel.split('.')
        .next()
        .expect("Could not find a '.'");
    let i = ImportantExcerpt { part: first_sentence };
}
```


The patterns programmed into Rust’s analysis of references are called the _lifetime elision rules_. Lifetimes on function or method parameters are called _input lifetimes_, and lifetimes on return values are called _output lifetimes_:

1. each parameter that is a reference gets its own lifetime parameter;

2. If there is exactly one input lifetime parameter, that lifetime is assigned to all output lifetime parameters.


3. If there are multiple input lifetime parameters, but one of them is `&self` or `&mut self` because this is a method, the lifetime of `self` is assigned to all output lifetime parameters. 

If the three rules are applied and the return parameter lifetime is still not determined, there is a compiler error.

One special lifetime we need to discuss is ``'static`, which means that this reference can live for the entire duration of the program.

```rust
let s: &'static str = "I have a static lifetime.";
```

The text of this string is stored directly in the program’s binary, which is always available. Before specifying 'static` as the lifetime for a reference, think about whether the reference you have actually lives the entire lifetime of your program or not.

# Functional Language Features: Iterators and Closures

## Closure

- closure: anonymous functions that can be saved in a variable or passed as arguments to other functions. Closures can capture values from the scope in which they are defined. Closures are not used in an exposed interface so type annotations are not required. Within limited contexts, the compiler is reliably able to infer the types of the parameters and the return type. Still, we can add type annotations if we want to increase explicitness and clarity at the cost of being more verbose than is strictly necessary.

```rust
let expensive_closure = |num: u32| -> u32 {
    println!("calculating slowly...");
    thread::sleep(Duration:from_secs(2));
    num
};

```

```rust
     let example_closure = |x| x ;

     let s = example_closure(String::from("hello"));
     let n = example_closure(5);
```

```bash
error[E0308]: mismatched types
 --> rust_scratachpad.rs:5:30
  |
5 |      let n = example_closure(5);
  |                              ^
  |                              |
  |                              expected struct `std::string::String`, found integer
  |                              help: try using a conversion method: `5.to_string()`
  |
  = note: expected type `std::string::String`
             found type `{integer}`
```

All closures implement at least one of the traits:

1. `Fn`;

2. `FnMut`;

3. `FnOnce`.

A lazy evaluation example

```rust
use std::collections::HashMap;

struct Cacher<T>
where T: Fn(u32) -> u32
{
    calculation: T,
    values: HashMap,
}

impl<T> Cacher<T>
where T: Fn(u32) -> u32
{
    fn new(calculation: T) -> Cacher<T> {
        Cacher {
            calculation,
            values: HashMap::new(),
        }
    }

    fn value(&mut self, arg: u32) -> u32 {
        match self.values.get(arg) {
            Some(v) => v,
            None => {
                let v = self.calculation(arg);
                self.values.insert(arg, v);
                v
            }
        }
    }
}
```

Closures can capture their environment and access variables from the scope in which they're defined in three traits:

1. `FnOnce`: consumes the variables it captures from its enclosing scope;

2. `FnMut`: mutably borrow values;


3. `Fn`: immutably borrow values.

Rust infers which trait to use based on how the closure uses the values from the environment. All closures implement `FnOnce` because they can all be called at least once. Closures that don’t move the captured variables also implement `FnMut`, and closures that don’t need mutable access to the captured variables also implement `Fn`. `move` keyword before the parameter list forces the closure to take ownership of the values it uses in the environment, useful when passing a closure to a new thread. Most of the time when specifying one of the `Fn` trait bounds, you can start with `Fn` and the compiler will tell you if you need `FnMut` or `FnOnce` based on what happens in the closure body.

## Iterators

In Rust, iterators are _lazy_, meaning they have no effect until methods that consume them are called to use it up. All iterators implement a trait named `Iterator`.

```rust
pub trait Iterator {
    type Item;              // associated type

    fn next(&mut self) -> Option<Self::Item>;
    

    // methods with default implementations elided
}
```

Implementing `Iterator` requries defining an `Item` type. Only `next()` is required for implementors. `vec.iter()` produces an iterator over immutable references. `.into_iter()` returns iterators taking ownership; `iter_mut()` returns iterators over mutable references.

- _Consuming adaptors_: methods that call `next()`.

- _iterator adaptors_: changes iterators into different kinds of iterators. Since iterators are lazy, consuming adaptors must be called (such as `.collect()`).

```rust
let v1: Vec<i32> = vec![1, 2, 3];
let v2: Vec<_> = v1.iter().map(|x| x + 1).collect();

fn shoes_in_my_size(shoes: Vec<Shoe>, shoe_size: u32) -> Vec<Shoe> {
    shoes.into_iter()
        .filter(|s| s.size == shoe_size)
        .collect()
}
```

An implementation:

```rust
struct Counter {
    count: u32
}

impl Counter {
    fn new() -> Counter {
        Counter { count: 0}
    }
}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        self.count += 1;

        if self.count < 6 {
            Some(self.count)
        } else {
            None
        }
    }
}

// now it's possible to use it with other Iterator methods
    let sum: u32 = Counter::new().zip(Counter::new().skip(1))
                                 .map(|(a, b)| a * b)
                                 .filter(|x| x % 3 == 0)
                                 .sum();

```

# Automated Tests

Rust includes support for writing automated software tests within the language. Tests are functions that verify that the non-test code is functioning in the expected manner. The bodies of test functions typically perform three actions:

1. Set up any needed data or state;

2. Run the code to test;

3. Assert the results are what are expected.
