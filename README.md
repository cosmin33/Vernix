# Vernix

Interpreter for a scala-like mini programming language

## Documentation

- **[Parser Documentation](PARSER.md)**: Comprehensive guide to the Vernix parser, including syntax, examples, and API usage

## Quick Start

### Using the Parser

Run the parser examples:
```bash
sbt "runMain io.vernix.Parsing"
```

Parse and execute a simple program:
```scala
import io.vernix.{Parsing, VarHeap}
import scala.util.Try

val source = """
  var x = 2 + 3 * 4
  var y = x - 5
  y
"""

Parsing.parseUnknown(source) match {
  case Right(program) => 
    println(program.execute[Try](VarHeap.empty))
  case Left(error) => 
    println(s"Error: $error")
}
```

### Using the DSL Directly

You can also create programs directly using the embedded DSL:

```scala
import io.vernix.Program.*
import zio.*

// Simple arithmetic
val program1 = value(2) + value(3) * value(4)
// Evaluate with ZIO: 14

// With variables
val program2 = 
  addVar("x", value(10)) *>
  addVar("y", value(20)) *>
  (variable[Int]("x") + variable[Int]("y"))
// Evaluate with ZIO: 30

// With control flow
val program3 = 
  addVar("x", value(5)) *>
  whileDo(variable[Int]("x") < value(10))(
    setVar("x", variable[Int]("x") + value(1))
  ) *>
  variable[Int]("x")
// Evaluate with ZIO: 10

// Execute a program
import zio.interop.catz.*
Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(program1.execute[Task]()).getOrThrow()
}
```

## Features

- **Type-safe DSL**: Create programs using a typed embedded DSL
- **Parser**: Parse text-based programs into executable AST
- **Multiple execution backends**: Execute with ZIO Task, Scala Try, or custom effect types
- **Control flow**: Support for if-then-else, while-do, and repeat-until loops
- **Variables**: Mutable variable support with type safety and scoping
- **Operators**: 
  - Arithmetic: `+`, `-`, `*`, `/`, `%`, `quot`
  - Comparison: `<`, `<=`, `>`, `>=`, `===`, `!==`
  - Logical: `&&`, `||`, `!`
  - String: `++` (concatenation), `len`
- **Type conversions**: `toDouble` for Int to Double conversion
- **Nested scopes**: Support for nested variable scopes with the `nest` operation
- **Comprehensive test suite**: 110+ tests covering all major functionality

## Project Structure

- `src/main/scala/io/vernix/`
  - `Parsing.scala`: Text parser implementation
  - `programs.scala`: Core Program DSL
  - `expressions.scala`: Compiled expression types
  - `ops.scala`: Operation implementations
  - `Type.scala`: Type system definitions
  - `ParsingOps.scala`: Parser helper operations
  - `VarHeap.scala`: Variable heap for managing variable state
  - `statements.scala`: Statement operations and implementations
  - `Expr01.scala`: Main demonstration application

- `src/test/scala/io/vernix/`
  - `ProgramSpec.scala`: Tests for Program DSL operations
  - `ExprSpec.scala`: Tests for Expr operations
  - `VariableSpec.scala`: Tests for variable management
  - `ControlFlowSpec.scala`: Tests for control flow constructs
  - `TypeSpec.scala`: Tests for type system
  - `VarHeapSpec.scala`: Tests for VarHeap operations

## Building

```bash
sbt compile
sbt test
sbt package
```

## Testing

Vernix includes a comprehensive test suite using ZIO Test. The test suite covers:

- **Program DSL operations** (27 tests): Arithmetic, string, boolean, comparison operations, type conversions, and sequencing
- **Variable management** (12 tests): Variable declaration, assignment, scoping, and error handling
- **Control flow** (11 tests): if-then-else, while-do, repeat-until loops, and complex control flow scenarios
- **Expression operations** (24 tests): All Expr operations including arithmetic, boolean logic, comparisons, and conversions
- **Type system** (20 tests): Basic types, complex types, nested types, and type inference
- **VarHeap operations** (16 tests): Variable management and scope management

### Running Tests

Run all tests:
```bash
sbt test
```

Run a specific test suite:
```bash
sbt "testOnly io.vernix.ProgramSpec"
```

Run tests with verbose output:
```bash
sbt "testOnly * -- -v"
```

### Test Coverage

The test suite provides comprehensive coverage of:
- Core DSL functionality across all supported types (Int, Double, Boolean, String)
- Variable scoping with nested contexts
- Control flow with loops and conditionals
- Type safety and type inference
- Error cases and edge conditions
- Integration between Program, Expr, and Statement layers
