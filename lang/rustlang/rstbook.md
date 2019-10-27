# Cargo and related

`Cargo.lock` ensures reproducible builds by recording versions of the dependencies when first building, instead of resolving dependencies whenever building. `cargo update` bypasses this.

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

- `if`: too many `else if` can be replaced by `match`. Since `if` is an expression:

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