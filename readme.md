# Krab

Krab is a simple and minimalistic interpreted programming language written in Rust. It has almost no functionality and it is not meant to be used for any serious purpose. It was created as a fun experiment to see how compact and simple a programming language can be.

## Features

- Basic types: int, float, bool, string
- Variables: let and const
- Control flow: if, while, break, continue
- Functions: fn and return
- Expressions: arithmetic, logical and comparison operators
- Native function: print()

## Syntax

Krab has a very simple syntax that resembles Rust. Here are some examples:

```rust
// Declare a variable with explicit type
let x: int = 42;

// Declare a variable with implicit type
let y = 3.14;

// Declare a constant
const z = true;

// Define a function
fn add(a: int, b: int) {
    // Return without semicolon
    a + b 
}

// Call a function
let result = add(x, 8);

// Print to stdout
print(result);

// Use if as an expression
let max = if x > y {
    x // No semicolon here!
} else {
    y // Or here!
};

// Use while loop for iteration
let i = 0;
while i < 10 {
    print(i);
    i = i + 1;
}
```

## Usage
To use Krab, you need to install [Rust](https://www.rust-lang.org/pt-BR/tools/install) and run `cargo run` in the project directory. 
This will launch a REPL where you can write Krab code interactively. You can also execute a .krab file by passing its path to the REPL. For example:

```bash
$ cargo run
- Krab 0.1 REPL
Type 'exit' to leave or 'clear' to clear the terminal
> ./code.krab
Hello from Krab!
> 5 + 5
10
> 
```

## Why Krab?
There is no good reason to use Krab over any other programming language. It is just a toy language that was made for fun and learning. If you are looking for an embedded scripting language for your Rust code, you should check out [rhai](https://github.com/rhaiscript/rhai) instead.

However, Krab is better than JavaScript! (but slower)