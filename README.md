# Vernix

Interpreter for a scala-like mini programming language

## Documentation

- **[Parser Documentation](PARSER.md)**: Comprehensive guide to the Vernix parser, including syntax, examples, API usage, and the [rendered output format](PARSER.md#rendered-output-format)

## Quick Start

### Using the Parser

Parse and execute a simple program:
```scala
import io.vernix.{Parsing, VarHeap}
import cats.Eval
import cats.data.EitherT

// A lazy, exception-capturing effect. `Try` must NOT be used here: it is
// eager, so its `Defer` instance evaluates the program as it is built
// rather than on demand, which breaks the interpreter. `Lazy` defers
// evaluation via `Eval` (it is `Eval[Either[Throwable, A]]` underneath).
type Lazy[A] = EitherT[Eval, Throwable, A]

val source = """
  var x = 2 + 3 * 4
  var y = x - 5
  y
"""

Parsing.parseUnknown(source) match {
  case Right(program) =>
    // `.value.value` unwraps EitherT -> Eval -> Either[Throwable, A]
    println(program.execute[Lazy](VarHeap.empty).value.value)
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

// With function definition and calling
val program4 = 
  addFunction[Int, Int]("triple", "x", variable[Int]("x") * value(3)) *>
  callFunction[Int, Int]("triple", value(5))
// Evaluate with ZIO: 15

// Execute a program
import zio.interop.catz.*
Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(program1.execute[Task]()).getOrThrow()
}
```

## Features

- **Type-safe DSL**: Create programs using a typed embedded DSL
- **Parser**: Parse text-based programs into executable AST
- **Multiple execution backends**: Execute with ZIO Task, a lazy `Lazy[A] = Eval[Either[Throwable, A]]`, or custom effect types (the backend must be lazy — eager types like `Try` are unsuitable)
- **Control flow**: Support for if-then-else, while-do, and repeat-until loops
- **Variables**: Mutable variable support with type safety and scoping
- **Operators**: 
  - Arithmetic: `+`, `-`, `*`, `/`, `%`, `quot`
  - Comparison: `<`, `<=`, `>`, `>=`, `===`, `!==`
  - Logical: `&&`, `||`, `!`
  - String: `++` (concatenation), `len`
- **Type conversions**: `toDouble` for Int to Double conversion
- **Functions**: Define and call single-parameter functions with type safety
- **Nested scopes**: Support for nested variable scopes with the `nest` operation
- **Tuples**: Tuple literals, element access, and destructuring `var (a, b) = ...` bindings
- **Pretty-printer**: Render any program back to source-like text with the `IdentState` interpreter
- **JSON codec**: Encode and decode programs with Circe via `ProgramCodec`
- **Comprehensive test suite**: 220 tests covering all major functionality

## Project Structure

- `src/main/scala/io/vernix/`
  - `Parsing.scala`: Text parser implementation
  - `programs.scala`: Core Program DSL
  - `ops.scala`: Operation implementations
  - `IdentState.scala`: Pretty-printing interpreter
  - `ProgramCodec.scala`: Circe JSON codec for programs
  - `Type.scala`: Type system definitions
  - `ParsingOps.scala`: Parser helper operations
  - `VarHeap.scala`: Variable heap for managing variable state
  - `statements.scala`: Statement operations and implementations
  - `Expr01.scala`: Main demonstration application

- `src/test/scala/io/vernix/`
  - `ProgramSpec.scala`: Tests for Program DSL operations
  - `ParsingSpec.scala`: Tests for the text parser
  - `VariableSpec.scala`: Tests for variable management
  - `ControlFlowSpec.scala`: Tests for control flow constructs
  - `TupleSpec.scala`: Tests for tuple operations
  - `IdentStateSpec.scala`: Tests for the pretty-printer output
  - `JsonCodecSpec.scala`: Tests for the JSON codec
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

- **Parser** (76 tests): Literals, operators, control flow, functions, tuples, type errors, rendering, and end-to-end programs
- **Program DSL operations** (27 tests): Arithmetic, string, boolean, comparison operations, type conversions, and sequencing
- **JSON codec** (28 tests): Round-tripping programs through Circe JSON
- **Type system** (19 tests): Basic types, complex types, nested types, and type inference
- **Tuples** (17 tests): Tuple construction, access, and rendering
- **VarHeap operations** (16 tests): Variable management and scope management
- **Pretty-printer** (14 tests): Rendered output format and indentation
- **Variable management** (12 tests): Variable declaration, assignment, scoping, and error handling
- **Control flow** (11 tests): if-then-else, while-do, repeat-until loops, and complex control flow scenarios

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
