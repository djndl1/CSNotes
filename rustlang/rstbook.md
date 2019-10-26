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

To ensure safety and easy concurrency, by default variables are immutable. `mut` makes a variable mutable. Constants are declared using `const` and its type must be annotated.

```rust
const MAX_POINTS: u32 = 100_000;
```

A new variable with the same name as a previous variable shadows the previous one.
