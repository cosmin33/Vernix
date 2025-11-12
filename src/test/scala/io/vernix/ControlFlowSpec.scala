package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.interop.catz.*
import Program.*

object ControlFlowSpec extends ZIOSpecDefault {
  def spec = suite("Control Flow")(
    suite("If-Else")(
      test("ifElse returns true branch when condition is true") {
        val program = ifElse(value(true))(
          value(42),
          value(24)
        )
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 42)
      },
      test("ifElse returns false branch when condition is false") {
        val program = ifElse(value(false))(
          value(42),
          value(24)
        )
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 24)
      },
      test("ifElse with comparison condition") {
        val program = 
          addVar("x", value(10)) *>
          ifElse(variable[Int]("x") > value(5))(
            value("greater"),
            value("not greater")
          )
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == "greater")
      },
      test("nested ifElse") {
        val program = 
          ifElse(value(true))(
            ifElse(value(false))(
              value(1),
              value(2)
            ),
            value(3)
          )
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 2)
      }
    ),
    suite("While Loop")(
      test("whileDo executes while condition is true") {
        val program = 
          addVar("counter", value(0)) *>
          whileDo(variable[Int]("counter") < value(5))(
            setVar("counter", variable[Int]("counter") + value(1))
          ) *>
          variable[Int]("counter")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 5)
      },
      test("whileDo does not execute when condition is initially false") {
        val program = 
          addVar("counter", value(10)) *>
          whileDo(variable[Int]("counter") < value(5))(
            setVar("counter", variable[Int]("counter") + value(1))
          ) *>
          variable[Int]("counter")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 10)
      },
      test("whileDo with accumulation") {
        val program = 
          addVar("sum", value(0)) *>
          addVar("i", value(1)) *>
          whileDo(variable[Int]("i") <= value(5))(
            setVar("sum", variable[Int]("sum") + variable[Int]("i")) *>
            setVar("i", variable[Int]("i") + value(1))
          ) *>
          variable[Int]("sum")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 15) // 1+2+3+4+5 = 15
      }
    ),
    suite("Repeat-Until Loop")(
      test("repeatUntil executes at least once") {
        val program = 
          addVar("counter", value(10)) *>
          repeatUntil(
            setVar("counter", variable[Int]("counter") + value(1)) *>
            variable[Int]("counter")
          )(value(true))
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 11)
      },
      test("repeatUntil loops until condition is met") {
        val program = 
          addVar("counter", value(0)) *>
          repeatUntil(
            setVar("counter", variable[Int]("counter") + value(1)) *>
            variable[Int]("counter")
          )(variable[Int]("counter") >= value(5))
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 5)
      },
      test("repeatUntil with accumulation") {
        val program = 
          addVar("sum", value(0)) *>
          addVar("i", value(0)) *>
          repeatUntil(
            setVar("i", variable[Int]("i") + value(1)) *>
            setVar("sum", variable[Int]("sum") + variable[Int]("i")) *>
            variable[Int]("i")
          )(variable[Int]("i") >= value(5)) *>
          variable[Int]("sum")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 15) // 1+2+3+4+5 = 15
      }
    ),
    suite("Complex Control Flow")(
      test("nested loops and conditionals") {
        val program = 
          addVar("result", value(0)) *>
          addVar("i", value(0)) *>
          whileDo(variable[Int]("i") < value(3))(
            ifElse(variable[Int]("i") % value(2) === value(0))(
              setVar("result", variable[Int]("result") + value(2)),
              setVar("result", variable[Int]("result") + value(1))
            ) *>
            setVar("i", variable[Int]("i") + value(1))
          ) *>
          variable[Int]("result")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 5) // i=0: +2, i=1: +1, i=2: +2 -> 2+1+2=5
      }
    )
  )
}
