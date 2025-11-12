package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.interop.catz.*

object ControlFlowSpec extends ZIOSpecDefault {
  def spec = suite("Control Flow")(
    suite("If-Else")(
      test("ifElse returns true branch when condition is true") {
        val program = Program.ifElse(Program.value(true))(
          Program.value(42),
          Program.value(24)
        )
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 42)
      },
      test("ifElse returns false branch when condition is false") {
        val program = Program.ifElse(Program.value(false))(
          Program.value(42),
          Program.value(24)
        )
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 24)
      },
      test("ifElse with comparison condition") {
        val program = 
          Program.addVar("x", Program.value(10)) *>
          Program.ifElse(Program.variable[Int]("x") > Program.value(5))(
            Program.value("greater"),
            Program.value("not greater")
          )
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == "greater")
      },
      test("nested ifElse") {
        val program = 
          Program.ifElse(Program.value(true))(
            Program.ifElse(Program.value(false))(
              Program.value(1),
              Program.value(2)
            ),
            Program.value(3)
          )
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 2)
      }
    ),
    suite("While Loop")(
      test("whileDo executes while condition is true") {
        val program = 
          Program.addVar("counter", Program.value(0)) *>
          Program.whileDo(Program.variable[Int]("counter") < Program.value(5))(
            Program.setVar("counter", Program.variable[Int]("counter") + Program.value(1))
          ) *>
          Program.variable[Int]("counter")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 5)
      },
      test("whileDo does not execute when condition is initially false") {
        val program = 
          Program.addVar("counter", Program.value(10)) *>
          Program.whileDo(Program.variable[Int]("counter") < Program.value(5))(
            Program.setVar("counter", Program.variable[Int]("counter") + Program.value(1))
          ) *>
          Program.variable[Int]("counter")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 10)
      },
      test("whileDo with accumulation") {
        val program = 
          Program.addVar("sum", Program.value(0)) *>
          Program.addVar("i", Program.value(1)) *>
          Program.whileDo(Program.variable[Int]("i") <= Program.value(5))(
            Program.setVar("sum", Program.variable[Int]("sum") + Program.variable[Int]("i")) *>
            Program.setVar("i", Program.variable[Int]("i") + Program.value(1))
          ) *>
          Program.variable[Int]("sum")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 15) // 1+2+3+4+5 = 15
      }
    ),
    suite("Repeat-Until Loop")(
      test("repeatUntil executes at least once") {
        val program = 
          Program.addVar("counter", Program.value(10)) *>
          Program.repeatUntil(
            Program.setVar("counter", Program.variable[Int]("counter") + Program.value(1)) *>
            Program.variable[Int]("counter")
          )(Program.value(true))
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 11)
      },
      test("repeatUntil loops until condition is met") {
        val program = 
          Program.addVar("counter", Program.value(0)) *>
          Program.repeatUntil(
            Program.setVar("counter", Program.variable[Int]("counter") + Program.value(1)) *>
            Program.variable[Int]("counter")
          )(Program.variable[Int]("counter") >= Program.value(5))
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 5)
      },
      test("repeatUntil with accumulation") {
        val program = 
          Program.addVar("sum", Program.value(0)) *>
          Program.addVar("i", Program.value(0)) *>
          Program.repeatUntil(
            Program.setVar("i", Program.variable[Int]("i") + Program.value(1)) *>
            Program.setVar("sum", Program.variable[Int]("sum") + Program.variable[Int]("i")) *>
            Program.variable[Int]("i")
          )(Program.variable[Int]("i") >= Program.value(5)) *>
          Program.variable[Int]("sum")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 15) // 1+2+3+4+5 = 15
      }
    ),
    suite("Complex Control Flow")(
      test("nested loops and conditionals") {
        val program = 
          Program.addVar("result", Program.value(0)) *>
          Program.addVar("i", Program.value(0)) *>
          Program.whileDo(Program.variable[Int]("i") < Program.value(3))(
            Program.ifElse(Program.variable[Int]("i") % Program.value(2) === Program.value(0))(
              Program.setVar("result", Program.variable[Int]("result") + Program.value(2)),
              Program.setVar("result", Program.variable[Int]("result") + Program.value(1))
            ) *>
            Program.setVar("i", Program.variable[Int]("i") + Program.value(1))
          ) *>
          Program.variable[Int]("result")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 5) // i=0: +2, i=1: +1, i=2: +2 -> 2+1+2=5
      }
    )
  )
}
