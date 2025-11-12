package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.interop.catz.*

object ProgramSpec extends ZIOSpecDefault {
  def spec = suite("Program")(
    suite("Arithmetic Operations")(
      test("addition of two integers") {
        val program = Program.value(5) + Program.value(3)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 8)
      },
      test("subtraction of two integers") {
        val program = Program.value(10) - Program.value(4)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 6)
      },
      test("multiplication of two integers") {
        val program = Program.value(6) * Program.value(7)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 42)
      },
      test("division of two doubles") {
        val program = Program.value(10.0) / Program.value(2.0)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 5.0)
      },
      test("modulo of two integers") {
        val program = Program.value(17) % Program.value(5)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 2)
      },
      test("negation of an integer") {
        val program = Program.value(42).neg
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == -42)
      },
      test("absolute value of a negative integer") {
        val program = Program.value(-42).abs
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 42)
      },
      test("complex arithmetic expression") {
        val program = (Program.value(2) + Program.value(3)) * Program.value(4)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 20)
      }
    ),
    suite("String Operations")(
      test("string concatenation") {
        val program = Program.value("Hello, ") ++ Program.value("World!")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == "Hello, World!")
      },
      test("string length") {
        val program = Program.value("Vernix").len
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 6)
      },
      test("empty string length") {
        val program = Program.value("").len
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 0)
      }
    ),
    suite("Boolean Operations")(
      test("logical AND with true values") {
        val program = Program.value(true) && Program.value(true)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == true)
      },
      test("logical AND with mixed values") {
        val program = Program.value(true) && Program.value(false)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == false)
      },
      test("logical OR with false values") {
        val program = Program.value(false) || Program.value(false)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == false)
      },
      test("logical OR with mixed values") {
        val program = Program.value(false) || Program.value(true)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == true)
      },
      test("logical NOT") {
        val program = !Program.value(true)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == false)
      },
      test("complex boolean expression") {
        val program = (Program.value(true) && Program.value(false)) || Program.value(true)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == true)
      }
    ),
    suite("Comparison Operations")(
      test("equality of equal integers") {
        val program = Program.value(42) === Program.value(42)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == true)
      },
      test("equality of different integers") {
        val program = Program.value(42) === Program.value(24)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == false)
      },
      test("inequality of different integers") {
        val program = Program.value(42) !== Program.value(24)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == true)
      },
      test("less than comparison") {
        val program = Program.value(5) < Program.value(10)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == true)
      },
      test("less than or equal comparison") {
        val program = Program.value(10) <= Program.value(10)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == true)
      },
      test("greater than comparison") {
        val program = Program.value(15) > Program.value(10)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == true)
      },
      test("greater than or equal comparison") {
        val program = Program.value(10) >= Program.value(15)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == false)
      }
    ),
    suite("Type Conversions")(
      test("integer to double conversion") {
        val program = Program.value(42).toDouble
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 42.0)
      }
    ),
    suite("Sequence Operations")(
      test("sequence two programs") {
        val program = Program.value(1) *> Program.value(2)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 2)
      },
      test("sequence three programs") {
        val program = Program.value(1) *> Program.value(2) *> Program.value(3)
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 3)
      }
    )
  )
}
