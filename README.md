# MicrOcaml Language Documentation

## Overview

MicroCaml is a minimalistic, dynamically-typed programming language, inspired by OCaml. It supports core functional programming concepts, such as first-class functions, conditional expressions, and recursive data structures. MicroCaml is interpreted, meaning that type checking and expression evaluation happen at runtime.

MicroCaml allows for flexible scripting with dynamic typing, making it ideal for learning functional programming principles and experimenting with functional constructs.

---

## mutop Usage

The `mutop` is an interactive shell for MicroCaml, similar to OCaml's `utop`. You can use it to evaluate expressions, define variables, and execute commands interactively.

### Example Session:

```ocaml
$ dune exec bin/mutop.exe

mutop # def a = 3;;
val a = Int 3

mutop # def b = 5;;
val b = Int 5

mutop # a * b;;
- : val: Int 15

mutop # let rec fact = fun x -> if x = 0 then 1 else x * fact (x - 1) in fact 5;;
- : val: Int 120
```

## Types

MicroCaml is dynamically typed, meaning that variable types are not declared and type checking is deferred until runtime. The following types are supported:

### 1. **Integer (`int`)**
  - Whole numbers, both positive and negative.
  - Example: `42`, `-7`
  
### 2. **Boolean (`bool`)**
  - True or false values.
  - Example: `true`, `false`

### 3. **String (`string`)**
  - Sequences of characters enclosed in double quotes.
  - Example: `"Hello, world!"`, `"MicroCaml"`

### 4. **Closures (`fun`)**
  - Functions that capture the environment in which they were defined.
  - Example: `fun x -> x + 1`

### 5. **Records**
  - Collections of labeled fields.
  - Example: `{ name = "Alice"; age = 30 }`

---

## Expressions

MicroCaml supports a wide variety of expressions, which can be evaluated in any context. Here are the primary expression types:

### 1. **Integer Expressions**
  - Integer values can be used directly.
  - Example: `5`

### 2. **Boolean Expressions**
  - Boolean values (`true` and `false`) represent logical values.
  - Example: `true`, `false`

### 3. **Arithmetic Operations**
  - MicroCaml supports basic arithmetic: addition (`+`), subtraction (`-`), multiplication (`*`), and division (`/`).
  - Example: 
    ```ocaml
    3 + 4 * 2
    ```

### 4. **Boolean Operations**
  - Logical operations include conjunction (`&&`), disjunction (`||`), and negation (`not`).
  - Example: 
    ```ocaml
    true && false
    not true
    ```

### 5. **Conditional Expressions**
  - The `if ... then ... else ...` construct allows conditional branching based on boolean expressions.
  - Example:
    ```ocaml
    if x > 3 then "yes" else "no"
    ```

### 6. **Let Bindings**
  - Bind values to variables with the `let ... in` construct. `let` creates a local binding that can be used within an expression.
  - Example:
    ```ocaml
    let x = 5 in x + 3
    ```

  - Recursive bindings can be created with `let rec`.
  - Example:
    ```ocaml
    let rec fact = fun x -> if x = 0 then 1 else x * (fact (x-1)) in fact 5
    ```

### 7. **Functions**
  - Anonymous functions can be created with the `fun` keyword. Functions can be stored in variables and applied to arguments.
  - Example:
    ```ocaml
    let f = fun x -> x + 1 in f 10
    ```

  - Functions can be applied by using parentheses for argument passing:
    ```ocaml
    (fun x -> x * 2) 3
    ```

### 8. **Binary Operations**
  - Binary operations include arithmetic and logical operators:
    - `+`, `-`, `*`, `/`: Arithmetic operators.
    - `=`, `<>`, `>`, `<`, `>=`, `<=`: Comparison operators.
    - `||`, `&&`: Logical operators.

  - Example:
    ```ocaml
    5 + 3 * 2
    true || false
    ```

### 9. **Record Expressions**
  - Records are used to store labeled values. Each field in a record has a name and a value.
  - Example:
    ```ocaml
    { name = "Alice"; age = 30 }
    ```

  - You can access fields using the `.` operator:
    ```ocaml
    person.name
    ```

---

## Functions

Functions can be passed as arguments, returned from other functions, or stored in data structures.

### 1. **Anonymous Functions**
  - Functions are defined using the `fun` keyword followed by the parameter and body.
  - Example:
    ```ocaml
    let add = fun x -> x + 1 in add 5
    ```

### 2. **Function Application**
  - Functions are applied by placing the argument after the function.
  - Example:
    ```ocaml
    let f = fun x -> x * x in f 4
    ```

### 3. **Recursive Functions**
  - Functions can be recursively defined using the `let rec` construct.
  - Example:
    ```ocaml
    let rec factorial = fun n -> if n = 0 then 1 else n * factorial (n - 1) in factorial 5
    ```

---

## Records

MicroCaml supports simple record structures, which are collections of named fields. Records are used to store related data together.

### 1. **Defining Records**
  - Records are defined using curly braces `{}` and contain a list of labeled fields.
  - Example:
    ```ocaml
    let person = { name = "Alice"; age = 25 }
    ```

### 2. **Accessing Record Fields**
  - You can access a field of a record using the `.` operator.
  - Example:
    ```ocaml
    person.name
    ```

---

## Examples

### 1. Factorial Function
```ocaml
let rec factorial = fun n -> if n = 0 then 1 else n * factorial (n - 1) in factorial 5
This calculates the factorial of 5, resulting in `120`.
```

### 2. Arithmetic and Conditional Expression
```ocaml
let x = 10 in if x > 5 then x + 2 else x - 2
```
This evaluates to `12` since `x > 5` is true.

### 3. Record Usage
```ocaml
let student = { name = "Alice"; grade = 90 } in student.name ^ " has grade " ^ string_of_int student.grade
This concatenates the student's name with their grade.
```

## Error Handling
MicroCaml throws runtime exceptions in case of errors. The most common errors are:

- **TypeError**: Raised when an operation is applied to incompatible types.
  - Example: Applying `+` to a string and an integer.
  
- **DeclareError**: Raised when an undefined variable is accessed.
  - Example: Accessing `x` without defining it.
  
- **DivByZeroError**: Raised when dividing by zero.
  
- **SelectError**: Raised when accessing a non-existent field in a record.

---

## Syntax Overview

### Grammar Summary

- **Values**: 
  - `int`: e.g., `1`, `-5`
  - `bool`: e.g., `true`, `false`
  - `string`: e.g., `"hello"`
  
- **Operations**:
  - Arithmetic: `+`, `-`, `*`, `/`
  - Boolean: `&&`, `||`, `not`
  - Comparison: `=`, `<>`, `>`, `<`, `>=`, `<=`
  
- **Control Flow**: `if ... then ... else`
  
- **Bindings**: `let ... in ...`, `let rec ... in ...`
  
- **Functions**: `fun x -> ...`
  
- **Records**: `{ field1 = value1; field2 = value2 }`
  
- **Access Fields**: `record.field`
