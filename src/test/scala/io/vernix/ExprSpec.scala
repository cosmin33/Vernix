package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.interop.catz.*

object ExprSpec extends ZIOSpecDefault {
  def spec = suite("Expr")(
    suite("Basic Expressions")(
      test("create and evaluate a simple integer expression") {
        val expr = Expr.value(42)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 42)
      },
      test("create and evaluate a simple string expression") {
        val expr = Expr.value("Hello")
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == "Hello")
      },
      test("create and evaluate a boolean expression") {
        val expr = Expr.value(true)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == true)
      }
    ),
    suite("Arithmetic Expressions")(
      test("addition expression") {
        val expr = Expr.value(10) + Expr.value(5)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 15)
      },
      test("subtraction expression") {
        val expr = Expr.value(20) - Expr.value(7)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 13)
      },
      test("multiplication expression") {
        val expr = Expr.value(6) * Expr.value(7)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 42)
      },
      test("division expression") {
        val expr = Expr.value(10.0) / Expr.value(2.0)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 5.0)
      },
      test("modulo expression") {
        val expr = Expr.value(17) % Expr.value(5)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 2)
      },
      test("complex arithmetic expression") {
        val expr = (Expr.value(2) + Expr.value(3)) * Expr.value(4)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 20)
      }
    ),
    suite("String Expressions")(
      test("string concatenation") {
        val expr = Expr.value("Hello, ") ++ Expr.value("World!")
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == "Hello, World!")
      },
      test("string length") {
        val expr = Expr.value("Vernix").len
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 6)
      }
    ),
    suite("Boolean Expressions")(
      test("logical AND") {
        val expr = Expr.value(true) && Expr.value(false)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == false)
      },
      test("logical OR") {
        val expr = Expr.value(false) || Expr.value(true)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == true)
      },
      test("logical NOT") {
        val expr = !Expr.value(true)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == false)
      }
    ),
    suite("Comparison Expressions")(
      test("equality") {
        val expr = Expr.value(42) === Expr.value(42)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == true)
      },
      test("inequality") {
        val expr = Expr.value(42) !== Expr.value(24)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == true)
      },
      test("less than") {
        val expr = Expr.value(5) < Expr.value(10)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == true)
      },
      test("greater than") {
        val expr = Expr.value(15) > Expr.value(10)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == true)
      }
    ),
    suite("Control Flow Expressions")(
      test("ifElse expression") {
        val expr = Expr.value(true).ifElse(
          Expr.value(42),
          Expr.value(24)
        )
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 42)
      },
      test("whileDo expression") {
        val expr = Expr.whileDo(Expr.value(false))(Expr.value(42))
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == ())
      }
    ),
    suite("Type Conversions")(
      test("integer to double conversion") {
        val expr = Expr.value(42).toDouble
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 42.0)
      }
    ),
    suite("Sequence Operations")(
      test("sequence two expressions") {
        val expr = Expr.value(1) *> Expr.value(2)
        for {
          result <- expr.apply[Task]
        } yield assertTrue(result == 2)
      }
    ),
    suite("Expr to Program Conversion")(
      test("convert Expr to Program") {
        val expr = Expr.value(42)
        val program = expr.prg
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 42)
      },
      test("convert complex Expr to Program") {
        val expr = (Expr.value(10) + Expr.value(5)) * Expr.value(2)
        val program = expr.prg
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 30)
      }
    )
  )
}
