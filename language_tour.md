# Wasm Types & Functions (WTF) Language Tour

Welcome to the Wasm Types & Functions (WTF) language tour! This guide will introduce you to the key features and syntax of WTF, a modern, concise programming language designed for WebAssembly. We'll explore the language through code examples, highlighting its capabilities and idiomatic usage.

---

## Table of Contents

- [Wasm Types \& Functions (WTF) Language Tour](#wasm-types--functions-wtf-language-tour)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Package Declaration](#package-declaration)
  - [Use Statements (`use`)](#use-statements-use)
  - [Comments](#comments)
  - [Variables](#variables)
    - [Immutable Variables (`let`)](#immutable-variables-let)
    - [Mutable Variables (`var`)](#mutable-variables-var)
  - [Basic Types](#basic-types)
  - [Records](#records)
  - [Resources](#resources)
  - [Enums](#enums)
  - [Variants (Sum Types)](#variants-sum-types)
  - [Functions](#functions)
    - [Function Declaration](#function-declaration)
    - [Function Parameters and Return Types](#function-parameters-and-return-types)
    - [Function Calls](#function-calls)
  - [Control Flow](#control-flow)
    - [`if` Statements](#if-statements)
    - [`while` Loops](#while-loops)
    - [`for` Loops](#for-loops)
  - [Option and Result Types](#option-and-result-types)
    - [Auto-Wrapping of Values](#auto-wrapping-of-values)
    - [Dealing with Optionals](#dealing-with-optionals)
    - [Error Handling](#error-handling)
      - [Return an error with `throw`](#return-an-error-with-throw)
      - [Error Propagation with the `!` Operator](#error-propagation-with-the--operator)
    - [`match` Statements](#match-statements)
  - [String interpolation](#string-interpolation)
  - [Built-ins](#built-ins)
    - [Operators](#operators)
    - [Types](#types)
  - [Conclusion](#conclusion)

---

## Introduction

WTF is a statically-typed language that emphasizes simplicity and expressiveness. It is designed to work seamlessly with WebAssembly, providing developers with powerful abstractions and a modern syntax.

## Package Declaration

Every WTF program starts with a package declaration that specifies the package name and, optionally, a version:

```wtf
package test:all_features@1.0.0;
```

- `package`: Keyword to declare a package.
- `test`: The package namespace.
- `all_features`: The package name.
- `1.0.0`: The package version.

The package declaration needs to be the first non-empty line in a .wtf source file.

## Use Statements (`use`)

Use the `use` keyword to bring types or functions from other packages into the namespace:

```wtf
use johndoe:math/math.{sqrt, pow}
use johndoe:io/printer.{print, println}
```

- `use`: Keyword for importing modules.
- `johndoe:math/math`: The module path, in the form `namespace:package/interface`. An interface is simply the source file in the package, where . Directories do not influence the package structure, so a file `math/src/advanced/algebra.wtf` would be accessed as `johndoe:math/algebra`.
- `{sqrt, pow}`: Specific items imported from the module.

Use statements are expected after the package declaration and before type or function declarations

## Comments

Single-line comments start with `//`:

```wtf
// This is a comment
```

Comments are used to explain code and are ignored by the compiler.

## Variables

### Immutable Variables (`let`)

Use `let` to declare an immutable variable:

```wtf
let origin = point {
    x: 0.0,
    y: 0.0
}
```

- `let`: Keyword for declaring an immutable variable.
- `origin`: Variable name.
- The value cannot be reassigned after initialization.

### Mutable Variables (`var`)

Use `var` to declare a mutable variable:

```wtf
var current_position = point {
    x: 10.0,
    y: 5.0
}
```

- `var`: Keyword for declaring a mutable variable.
- `current_position`: Variable name.
- The value can be reassigned.

## Basic Types

WTF includes several basic types:

- `s8`, `s16`, `s32`, `s64`: Signed integers of various sizes.
- `f32`, `f64`: Floating-point numbers.
- `bool`: Boolean type (`true` or `false`).
- `string`: Unicode strings.

## Records

Records are custom data structures with named fields:

```wtf
record point {
    x: f64
    y: f64
}
```

- `record`: Keyword to define a record.
- `point`: Name of the record.
- Fields are defined with their types.
- Records are structurally typed, meaning that a `record point3d { x: f64, y: f64, z: f64 }` would still conform to `point` and can be passed to functions where `point` is expected.
- Records are compared by value. `a` and `b` are equal if `a` has all fields `b` has and vice versa and each field of `a` equals the corresponding field in `b` 

**Creating an Instance:**

```wtf
let origin = point {
    x: 0.0,
    y: 0.0
}
```

Access fields using the dot notation:

```wtf
let x_coord = origin.x
```

## Resources

Resources are similar to classes with encapsulated data and associated methods:

```wtf
resource counter {
    value: s32

    constructor(initial: s32) {
        self.value = initial
    }

    func increment() {
        self.value += 1
    }

    func get_value() -> s32 {
        return self.value
    }
}
```

- `resource`: Keyword to define a resource.
- `constructor`: Special function to initialize the resource.
- `self`: Reference to the current instance.
Fields of classes are always private, methods of classes always have the same visibility as the class by default <- this is up for discussion and might change.

**Using a Resource:**

```wtf
var cnt = counter(0)
cnt.increment()
let val = cnt.get_value()
```

**Implementing a resource:**
```wtf
resource vehicle {
    func drive()
}

resource car {
    func drive() { 
        // ...
    }

    func honk() {
        // ...
    }
}

resource electric_car {
    func drive() {
        // ...
    }

    func honk() {
        // ...
    }

    func charge() {
        //...
    }
}
```
Similar to structural typing in records, resources can also conform to other resources. In the example above, a `car` can be passed whereever a `vehicle` is expected, as `car` implements all methods of `vehicle`. Note that `vehicle` here is an abstract resource: It does not implement any of its methods. Resources must either implement all or none of their methods.
This mechanism also works for concrete resources, so an `electric_car` can be used whenever a `car` is expected.

## Enums

Enums define a type with a fixed set of constant values:

```wtf
enum direction {
    north
    east
    south
    west
}
```

- `enum`: Keyword to define an enum.
- `direction`: Name of the enum.
- enum cases are defined without associated values.

**Using an Enum:**

```wtf
let dir = east
```

## Variants (Sum Types)

Variants are sum types that can hold different types of associated data:

```wtf
variant color {
    rgb(r: s8, g: s8, b: s8)
    grayscale(luminance: s8)
}
```

- `variant`: Keyword to define a variant.
- `color`: Name of the variant.
- Cases can have associated data.

**Creating a Variant Instance:**

```wtf
let red = rgb(255, 0, 0)
```

## Functions

### Function Declaration

Define functions using the `func` keyword:

```wtf
func calculate_distance(p1: point, p2: point) -> f64 {
    // Function body
}
```

- `func`: Keyword to declare a function.
- `calculate_distance`: Function name.

### Function Parameters and Return Types

- Parameters are defined with their names and types.
- Return type is specified after `->`.

**Example:**

```wtf
func add(a: s32, b: s32) -> s32 {
    return a + b
}
```

### Function Calls

Call functions by using their name and passing arguments:

```wtf
let sum = add(5, 7)
```

## Control Flow

### `if` Statements

Conditional execution using `if` and optional `else`:

```wtf
if cnt.value > 10 {
    println("Counter is greater than 10")
} else {
    println("Counter is less than or equal to 10")
}
```

- Condition is specified after `if`.
- Code blocks are enclosed in `{}`.

### `while` Loops

Execute a block of code while a condition is true:

```wtf
while cnt.value < 5 {
    cnt.increment()
    if cnt.value == 3 {
        continue
    }
    println("Counter in loop: " + cnt.value.to_string())
}
```

- `continue`: Skips to the next iteration.
- `break`: Exits the loop.

### `for` Loops

Iterate over a range or collection:

```wtf
for i in 0..<5 {
    println("For loop iteration: " + i.to_string())
    if i == 2 {
        break  // Break without value
    }
}
```

- `in`: Keyword to specify the range or collection.
- `0..<5`: Range from 0 to 4.

## Option and Result Types

### Auto-Wrapping of Values

Values can be automatically wrapped into `option` or `result` types.

**Option Type:**

```wtf
let optional_value: option<s32> = 42  // Automatically wrapped as `some(42)`
```

- `option<T>`: Represents a value that may or may not be present.
- `none`: Represents the absence of a value.

**Result Type:**

```wtf
return a / b  // Automatically wrapped as `ok(a / b)`
```

- `result<T, E>`: Represents either a success (`ok`) or an error (`err`).

### Dealing with Optionals
**Optional Chaining:** 
```wtf
let value = optional_value?.as_ascii()?.uppercase()

```

- If `optional_value` is `none`, the expression evaluates to `none`.
- If `optional_value` is `some`, the method `as_ascii()` is called.

**Null-coalescing operator:**
```wtf
let value: s32 = maybe_number ? 5
```

**Optional assignment operator**
`value ?= 5` is equivalent to `value = value ? 5`, thus it only performs an assignment if `value == none`
```wtf
var value: s32 = 5
value ?= 6 // value is still 5
value = none // value is none
value ?= 42 // value is 42
```

### Error Handling 

#### Return an error with `throw`

Use `throw` to return an error from a function:

```wtf
if b == 0 {
    throw "Division by zero"
}
```

- `throw`: Returns an error without explicitly wrapping in `err`.

#### Error Propagation with the `!` Operator

Use the `!` operator to propagate errors:

```wtf
let computation = compute(10, 0, 5)!
```

- The `!` operator (yeet operator) propagates an error to the calling function if one occurs.
- Simplifies error handling in nested function calls.

**Function with Error Propagation:**

```wtf
func compute(a: s32, b: s32, c: s32) -> result<s32, string> {
    let div = divide(a, b)!
    let result = div + c
    return result
}
```

### `match` Statements

Pattern matching with `match`:

```wtf
match dir {
    north => println("Going north")
    east => println("Going east")
    let other => println("Going \(other)")
}
```

- `match`: Keyword for pattern matching.
- Patterns are specified before `=>`.
- Supports variable binding with `let` at arbitrary positions in the pattern:

```wtf
match maybe {
    some(let inner) => inner,
    none => 42,
}
```

## String interpolation
As seen in the last section, strings support string interpolation:
```wtf
func say_hello(name: string?) -> string {
    "Hello, \(name ? "World")"
}
```
- `\()` is used for string interpolation

**Format rules use `:`**
```wtf
let rough_pi = "Pi is roughly \(PI:.2f)" // "Pi is roughly 3.14"
```
- The standard format rule is `:v`, which prints the value in its display format

```wtf
let point = { x: 0.8, y: 1.0, z: 1.2 }
print("Point: \(point:#v)") 
// Point: { x: 0.8, y: 1.0, z: 1.2 }
```
- Use `:#v` to print the value in debug syntax format


## Built-ins
### Operators
- Arithmetic Operators: `+`, `-`, `*`, `/`
- Comparison Operators: `==`, `!=`, `>`, `<`, `>=`, `<=`
- Logical Operators: `&&`, `||`, `!`
- Assignment Operators: `=`, `+=`, `-=`, `*=`, `/=`
- Range Operator: `..<` or `..=` (for loops)
- Safe Access Operator: `?.` (for optional chaining)

### Types
| Type Name     | Shorthand | Description                                      |
|---------------|-----------|--------------------------------------------------|
| `s8`          |           | 8-bit signed integer                             |           
| `s16`         |           | 16-bit signed integer                            |           
| `s32`         |           | 32-bit signed integer                            |           
| `s64`         |           | 64-bit signed integer                            |           
| `u8`          |           | 8-bit unsigned integer                           |           
| `u16`         |           | 16-bit unsigned integer                          |           
| `u32`         |           | 32-bit unsigned integer                          |           
| `u64`         |           | 64-bit unsigned integer                          |           
| `f32`         |           | 32-bit floating-point number                     |           
| `f64`         |           | 64-bit floating-point number                     |           
| `bool`        |           | Boolean type (`true` or `false`)                 |           
| `char`        |           | Unicode character                                |   
| `string`      |           | Unicode string                                   |   
| `list<T>`     | `[T]`     | Generic list of elements of type `T`             |
| `option<T>`   | `T?`      | Optional value of type `T` (`some(T)` or `none`) |
| `result<T,E>` | `T!E`     | Result type with value `T` or error `E`          |

## Conclusion

This tour introduced the core features of the WTF programming language, including variables, data structures, control flow, functions, and error handling. With its concise syntax and modern features, WTF aims to provide an efficient and expressive language for WebAssembly development.

To further explore WTF, consider writing your own programs, experimenting with different features, and building projects that leverage its capabilities.

Happy coding with WTF!
