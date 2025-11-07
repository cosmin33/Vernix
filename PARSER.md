# Vernix Parser Documentation

## Overview

The Vernix parser provides functionality to parse and execute a Scala-like mini programming language. It uses the [fastparse](https://github.com/com-lihaoyi/fastparse) library to parse source code text into executable `Program` objects that can be compiled and executed.

## Key Components

### Main Parser Object: `io.vernix.Parsing`

The parser is implemented in `Parsing.scala` and provides:
- **`parseUnknown(s: String)`**: Parses source code and returns `Either[String, Program[?]]`
- **`parseRun(s: String)`**: Utility method that parses and displays the program in various forms
- **`main(args: Array[String])`**: Demonstrates the parser with example programs

## Supported Language Features

### Data Types

The parser supports the following data types:

- **Int**: Integer literals (e.g., `42`, `-10`, `+5`)
- **Double**: Floating-point literals (e.g., `3.14`, `2.0d`, `.5d`)
- **Boolean**: `true` and `false`
- **Variables**: Named references to stored values

### Literals

```scala
// Integer literals
42
-10
+123

// Double literals
3.14
2.0d
.5d
-1.23

// Boolean literals
true
false
```

### Variables

#### Variable Declaration

Use `var` keyword to declare and initialize a variable:

```scala
var x = 10
var name = 2 + 3
```

#### Variable Assignment

Assign new values to existing variables:

```scala
x = x + 5
x = 20
```

#### Variable References

Variables can be used in expressions:

```scala
var y = x * 2
```

### Operators

#### Arithmetic Operators

The parser supports standard arithmetic operations with proper precedence:

- **Multiplication, Division, Modulo**: `*`, `/`, `%` (highest precedence)
- **Addition, Subtraction**: `+`, `-`

```scala
var result = 2 + 3 * 4      // Result: 14 (multiplication first)
var quotient = 10 / 3       // Result: 3 (integer division)
var remainder = 10 % 3      // Result: 1
```

#### Comparison Operators

- **Less than**: `<`
- **Less than or equal**: `<=`
- **Greater than**: `>`
- **Greater than or equal**: `>=`
- **Equal**: `==`
- **Not equal**: `!=`

```scala
var check = x > 10
var isEqual = y == 5
```

#### Logical Operators

- **AND**: `&`
- **OR**: `|`

```scala
var condition = x > 5 & y < 10
var altCondition = x == 0 | y == 0
```

### Control Flow

#### If-Then-Else

Conditional execution with optional `then` keyword:

```scala
if condition then
  statement
else
  statement
```

Example:
```scala
if y > 10 then
  x = x + 10
else
  x = x - 10
```

#### While-Do Loop

Execute a statement repeatedly while a condition is true:

```scala
while condition do
  statement
```

Example:
```scala
while x < 20 do
  x = x + 2
```

#### Repeat-Until Loop

Execute a statement at least once, then repeat until a condition becomes true:

```scala
repeat
  statement
until condition
```

Example:
```scala
repeat
  x = x + 3
until x >= 30
```

### Blocks

Multiple statements can be grouped in blocks using curly braces:

```scala
{
  var x = 5
  x = x + 1
  x
}
```

Statements within blocks are separated by semicolons or newlines.

## Parser API

### parseUnknown

Parses a string and returns the parsed program or an error:

```scala
def parseUnknown(s: String): Either[String, Program[?]]
```

**Returns:**
- `Right(program)`: Successfully parsed program
- `Left(errorMessage)`: Parse error with detailed message

**Example:**
```scala
val result = Parsing.parseUnknown("var x = 2 + 3")
result match {
  case Right(program) => println("Parse successful!")
  case Left(error) => println(s"Parse error: $error")
}
```

### parseRun

Utility method that parses and displays the program in three forms:

```scala
def parseRun(s: String): Unit
```

**Output format:**
1. **String representation**: Shows the program structure
2. **Applied form**: Shows the program after type application
3. **Execution result**: Shows the result of executing the program with `Try`

## Example Programs

### Example 1: Basic Variables and Arithmetic

**Source Code:**
```scala
var x = 2 + 3 * 4
var y = x - 5 / 2
```

**Explanation:**
- Declares variable `x` and initializes it to `2 + 3 * 4` (equals 14)
- Declares variable `y` and initializes it to `x - 5 / 2` (equals 12, since 5/2 = 2 in integer division)

**Output:**
```
Right(var x: Int = (2 + (3 * 4))
var y: Int = (x - (5 / 2)))
```

### Example 2: Complete Program with Control Flow

**Source Code:**
```scala
var x = 2 + 3 * 4
var y = x - 5 / 2
x = x - 10
if y > 10 then
  x = x + 10
else
  x = x - 10
while x < 20 do
  x = x + 2
repeat
  x = x + 3
until x >= 30
x
```

**Execution trace:**
1. `x = 14` (from `2 + 3 * 4`)
2. `y = 12` (from `14 - 5 / 2` = `14 - 2`)
3. `x = 4` (from `14 - 10`)
4. Since `y > 10` is true, `x = 14` (from `4 + 10`)
5. While loop: `x` increments by 2 until reaching 20: `14 → 16 → 18 → 20`
6. Repeat-until: `x` increments by 3 until `>= 30`: `20 → 23 → 26 → 29 → 32`
7. Returns `32`

**Output:**
```
Right(Success(32))
```

## Operator Precedence

From highest to lowest precedence:

1. **Literals and Variables**: `42`, `x`, `true`
2. **Parentheses**: `( ... )`
3. **Multiplicative**: `*`, `/`, `%`
4. **Additive**: `+`, `-`
5. **Comparison**: `<`, `<=`, `>`, `>=`, `==`, `!=`
6. **Logical**: `&`, `|`
7. **Assignment**: `=`
8. **Control Flow**: `if-then-else`, `while-do`, `repeat-until`

## Type System

The parser includes automatic type inference and checking:

### Supported Types

- **Int**: Integer numbers
- **Double**: Floating-point numbers
- **Boolean**: True/false values
- **String**: Text strings (via type system, limited parser support)
- **Unit**: Represents no value (from statements)

### Type Coercion

The parser handles type coercion for numeric operations:
- `Int` + `Double` → `Double`
- `Double` + `Int` → `Double`
- `Int` operations → `Int`
- `Double` operations → `Double`

### Type Checking

The parser enforces type constraints:
- Conditions in `if`, `while`, `repeat-until` must be `Boolean`
- Arithmetic operations require numeric types (`Int` or `Double`)
- Boolean operators (`&`, `|`) require `Boolean` operands
- Variables maintain their declared type

## Error Handling

The parser provides detailed error messages for:
- Syntax errors (unexpected tokens, missing keywords)
- Type errors (incompatible types in operations)
- Undefined variables
- Invalid operator usage

**Example error:**
```scala
Parsing.parseUnknown("var x = 5 + true")  // Type error: cannot add Int and Boolean
```

## Running the Parser

### From SBT

```bash
sbt "runMain io.vernix.Parsing"
```

### Programmatically

```scala
import io.vernix.{Parsing, VarHeap}
import scala.util.Try

// Parse a program
val source = """
  var x = 10
  x = x * 2
  x
"""

Parsing.parseUnknown(source) match {
  case Right(program) =>
    // Execute the program
    val result = program.execute[Try](VarHeap.empty)
    println(s"Result: $result")
  case Left(error) =>
    println(s"Parse error: $error")
}
```

## Implementation Details

### Parser Architecture

The parser is built using `fastparse` with the following components:

- **Lexical elements**: Character predicates for identifiers, operators, whitespace
- **Expression parsers**: Recursive descent parser for expressions with precedence
- **Statement parsers**: Parsers for control flow and variable statements
- **Type-aware parsing**: Integration with the Vernix type system via `Prog.Aux[T]`

### Key Parser Functions

- `factor`: Parses atomic expressions (literals, variables, parenthesized expressions)
- `divMul`: Handles `*`, `/`, `%` with left associativity
- `additionSubtraction`: Handles `+`, `-` with left associativity
- `comparison`: Handles comparison operators
- `andOr`: Handles logical operators
- `statement`: Dispatches to control flow or expression parsers
- `block`: Parses statement blocks
- `prog`: Top-level program parser

### Helper Classes

**`Prog` trait**: A type-erased wrapper that preserves runtime type information:
```scala
trait Prog {
  type T
  def `type`: Type[T]
  def program: Program[T]
}
```

This allows the parser to work with heterogeneous program types while maintaining type safety.

## Limitations

Current parser limitations:

1. **String literals**: Not yet implemented in the parser (type system supports strings)
2. **Function definitions**: The `def` keyword is defined but function parsing is not yet implemented
3. **Comments**: No support for code comments
4. **Arrays/Collections**: Not supported in the parser
5. **Import statements**: No module/import system

## Future Extensions

Potential enhancements for the parser:

- String literal parsing and operations
- Function definitions and calls
- Lambda expressions
- Pattern matching
- Collection literals and operations
- Comment support
- Error recovery for better error messages
- Source location tracking for debugging
