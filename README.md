# Vernix

Interpreter for a scala-like mini programming language

## Documentation

- **[Parser Documentation](PARSER.md)**: Comprehensive guide to the Vernix parser, including syntax, examples, and API usage

## Quick Start

Run the parser examples:
```bash
sbt "runMain io.vernix.Parsing"
```

Parse and execute a simple program:
```scala
import io.vernix.Parsing
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

## Features

- **Type-safe DSL**: Create programs using a typed embedded DSL
- **Parser**: Parse text-based programs into executable AST
- **Multiple execution backends**: Execute with ZIO Task, Scala Try, or custom effect types
- **Control flow**: Support for if-then-else, while-do, and repeat-until loops
- **Variables**: Mutable variable support with type safety
- **Operators**: Arithmetic, comparison, and logical operators with proper precedence

## Project Structure

- `src/main/scala/io/vernix/`
  - `Parsing.scala`: Text parser implementation
  - `programs.scala`: Core Program DSL
  - `expressions.scala`: Compiled expression types
  - `ops.scala`: Operation implementations
  - `Type.scala`: Type system definitions
  - `ParsingOps.scala`: Parser helper operations

## Building

```bash
sbt compile
sbt test
sbt package
```
